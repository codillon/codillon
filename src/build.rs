// build.rs - Auto-generates the list of WebAssembly instruction names from the wast crate source. Made with help of Gemini 3.1pro.
use std::{collections::BTreeSet, env, fs, path::Path};

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest = Path::new(&out_dir).join("wasm_instrs.rs");
    let wast_src = find_wast_src();
    let names = extract_instr_names(&wast_src);

    let mut code = String::new();
    code.push_str("pub const WASM_INSTR_NAMES: &[&str] = &[\n");
    for name in &names {
        code.push_str(&format!("    {:?},\n", name));
    }
    code.push_str("];\n");

    fs::write(&dest, code).unwrap();
    println!("cargo:rerun-if-changed=build.rs");
}

fn find_wast_src() -> String {
    let out_dir = env::var("OUT_DIR").unwrap();
    let mut path = Path::new(&out_dir).to_path_buf();
    for _ in 0..20 {
        path = match path.parent() {
            Some(p) => p.to_path_buf(),
            None => break,
        };
        let cargo_dir = path.join(".cargo");
        if cargo_dir.exists()
            && let Some(result) = search_cargo_registry(&cargo_dir)
        {
            return result;
        }
    }
    if let Ok(home) = env::var("HOME") {
        let cargo_dir = Path::new(&home).join(".cargo");
        if let Some(result) = search_cargo_registry(&cargo_dir) {
            return result;
        }
    }
    panic!("Could not find wast crate source");
}

fn search_cargo_registry(cargo_dir: &Path) -> Option<String> {
    let registry_src = cargo_dir.join("registry").join("src");
    if !registry_src.exists() {
        return None;
    }
    for index_dir in fs::read_dir(&registry_src).ok()? {
        let index_dir = index_dir.ok()?.path();
        for entry in fs::read_dir(&index_dir).ok()? {
            let entry = entry.ok()?.path();
            let name = entry.file_name()?.to_str()?;
            if name.starts_with("wast-") {
                let expr_rs = entry.join("src").join("core").join("expr.rs");
                if expr_rs.exists()
                    && let Ok(contents) = fs::read_to_string(&expr_rs)
                {
                    return Some(contents);
                }
            }
        }
    }
    None
}

fn extract_instr_names(src: &str) -> BTreeSet<String> {
    let mut names = BTreeSet::new();
    for line in src.lines() {
        let line = line.trim();
        if let Some(pos) = line.find("] : \"") {
            let after = &line[pos + 5..];
            let end = after.find('"').unwrap_or(after.len());
            let name = &after[..end];
            if !name.is_empty() && !name.contains(' ') {
                names.insert(name.to_string());
            }
        }
    }
    names
}
