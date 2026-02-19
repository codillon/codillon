// autocomplete.rs
const WASM_INSTRS: &[&str] = &[
    "block",
    "br",
    "br_if",
    "br_table",
    "call",
    "call_indirect",
    "drop",
    "else",
    "end",
    "f32.abs",
    "f32.add",
    "f32.const",
    "f32.div",
    "f32.load",
    "f32.mul",
    "f32.store",
    "f32.sub",
    "f64.abs",
    "f64.add",
    "f64.const",
    "f64.div",
    "f64.load",
    "f64.mul",
    "f64.store",
    "f64.sub",
    "global.get",
    "global.set",
    "i32.add",
    "i32.and",
    "i32.const",
    "i32.div_s",
    "i32.div_u",
    "i32.eq",
    "i32.load",
    "i32.lt_s",
    "i32.mul",
    "i32.or",
    "i32.rem_s",
    "i32.shl",
    "i32.store",
    "i32.sub",
    "i32.xor",
    "i64.add",
    "i64.and",
    "i64.const",
    "i64.div_s",
    "i64.eq",
    "i64.load",
    "i64.mul",
    "i64.or",
    "i64.shl",
    "i64.store",
    "i64.sub",
    "i64.xor",
    "if",
    "local.get",
    "local.set",
    "local.tee",
    "loop",
    "memory.grow",
    "memory.size",
    "nop",
    "return",
    "select",
    "unreachable",
];

pub fn suggest(prefix: &str, limit: usize) -> Vec<&'static str> {
    if prefix.is_empty() {
        return Vec::new();
    }
    let idx = WASM_INSTRS.partition_point(|&s| s < prefix);
    WASM_INSTRS[idx..]
        .iter()
        .copied()
        .take_while(|&s| s.starts_with(prefix))
        .filter(|&s| s != prefix)
        .take(limit)
        .collect()
}

pub fn completion_suffix(full: &'static str, prefix: &str) -> &'static str {
    &full[prefix.trim_start().len()..]
}
