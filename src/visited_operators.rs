// Helped make with Claude Sonnet 4.6
use wasmparser::for_each_operator;

macro_rules! define_visited_operators {
    ($( @$payload:ident $op:ident $({ $($arg:ident: $argty:ty),* })? => $visit:ident ($($arity:tt)*) )*) => {
        pub const VISITED_OPERATOR_NAMES: &[&str] = &[
            $(
                stringify!($visit),
            )*
        ];
    }
}

for_each_operator!(define_visited_operators);

pub fn get_all_instruction_names() -> Vec<String> {
    VISITED_OPERATOR_NAMES
        .iter()
        .map(|name| {
            let name = name.strip_prefix("visit_").unwrap_or(name);
            // Spec-defined prefixes that use a dot separator
            let dotted_prefixes = [
                "i32", "i64", "f32", "f64", "v128", "memory", "table", "global", "local", "ref",
                "elem", "data",
            ];

            for prefix in dotted_prefixes {
                if name.starts_with(prefix) && name.as_bytes().get(prefix.len()) == Some(&b'_') {
                    // Replace the first underscore with a dot
                    let mut new_name = String::with_capacity(name.len());
                    new_name.push_str(prefix);
                    new_name.push('.');
                    new_name.push_str(&name[prefix.len() + 1..]);
                    return new_name;
                }
            }

            name.to_string()
        })
        .collect()
}
