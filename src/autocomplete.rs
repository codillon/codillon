// autocomplete.rs - WebAssembly instruction autocomplete for Codillon

/// All valid WebAssembly text format instructions, sorted for binary search.
const WASM_INSTRS: &[&str] = &[
    "block",
    "br",
    "br_if",
    "br_table",
    "call",
    "call_indirect",
    "data.drop",
    "drop",
    "elem.drop",
    "else",
    "end",
    "f32.abs",
    "f32.add",
    "f32.ceil",
    "f32.const",
    "f32.convert_i32_s",
    "f32.convert_i32_u",
    "f32.convert_i64_s",
    "f32.convert_i64_u",
    "f32.copysign",
    "f32.demote_f64",
    "f32.div",
    "f32.eq",
    "f32.floor",
    "f32.ge",
    "f32.gt",
    "f32.le",
    "f32.load",
    "f32.lt",
    "f32.max",
    "f32.min",
    "f32.mul",
    "f32.ne",
    "f32.nearest",
    "f32.neg",
    "f32.reinterpret_i32",
    "f32.sqrt",
    "f32.store",
    "f32.sub",
    "f32.trunc",
    "f64.abs",
    "f64.add",
    "f64.ceil",
    "f64.const",
    "f64.convert_i32_s",
    "f64.convert_i32_u",
    "f64.convert_i64_s",
    "f64.convert_i64_u",
    "f64.copysign",
    "f64.div",
    "f64.eq",
    "f64.floor",
    "f64.ge",
    "f64.gt",
    "f64.le",
    "f64.load",
    "f64.lt",
    "f64.max",
    "f64.min",
    "f64.mul",
    "f64.ne",
    "f64.nearest",
    "f64.neg",
    "f64.promote_f32",
    "f64.reinterpret_i64",
    "f64.sqrt",
    "f64.store",
    "f64.sub",
    "f64.trunc",
    "global.get",
    "global.set",
    "i32.add",
    "i32.and",
    "i32.clz",
    "i32.const",
    "i32.ctz",
    "i32.div_s",
    "i32.div_u",
    "i32.eq",
    "i32.eqz",
    "i32.extend16_s",
    "i32.extend8_s",
    "i32.ge_s",
    "i32.ge_u",
    "i32.gt_s",
    "i32.gt_u",
    "i32.le_s",
    "i32.le_u",
    "i32.load",
    "i32.load16_s",
    "i32.load16_u",
    "i32.load8_s",
    "i32.load8_u",
    "i32.lt_s",
    "i32.lt_u",
    "i32.mul",
    "i32.ne",
    "i32.or",
    "i32.popcnt",
    "i32.reinterpret_f32",
    "i32.rem_s",
    "i32.rem_u",
    "i32.rotl",
    "i32.rotr",
    "i32.shl",
    "i32.shr_s",
    "i32.shr_u",
    "i32.store",
    "i32.store16",
    "i32.store8",
    "i32.sub",
    "i32.trunc_f32_s",
    "i32.trunc_f32_u",
    "i32.trunc_f64_s",
    "i32.trunc_f64_u",
    "i32.trunc_sat_f32_s",
    "i32.trunc_sat_f32_u",
    "i32.trunc_sat_f64_s",
    "i32.trunc_sat_f64_u",
    "i32.wrap_i64",
    "i32.xor",
    "i64.add",
    "i64.and",
    "i64.clz",
    "i64.const",
    "i64.ctz",
    "i64.div_s",
    "i64.div_u",
    "i64.eq",
    "i64.eqz",
    "i64.extend16_s",
    "i64.extend32_s",
    "i64.extend8_s",
    "i64.extend_i32_s",
    "i64.extend_i32_u",
    "i64.ge_s",
    "i64.ge_u",
    "i64.gt_s",
    "i64.gt_u",
    "i64.le_s",
    "i64.le_u",
    "i64.load",
    "i64.load16_s",
    "i64.load16_u",
    "i64.load32_s",
    "i64.load32_u",
    "i64.load8_s",
    "i64.load8_u",
    "i64.lt_s",
    "i64.lt_u",
    "i64.mul",
    "i64.ne",
    "i64.or",
    "i64.popcnt",
    "i64.reinterpret_f64",
    "i64.rem_s",
    "i64.rem_u",
    "i64.rotl",
    "i64.rotr",
    "i64.shl",
    "i64.shr_s",
    "i64.shr_u",
    "i64.store",
    "i64.store16",
    "i64.store32",
    "i64.store8",
    "i64.sub",
    "i64.trunc_f32_s",
    "i64.trunc_f32_u",
    "i64.trunc_f64_s",
    "i64.trunc_f64_u",
    "i64.trunc_sat_f32_s",
    "i64.trunc_sat_f32_u",
    "i64.trunc_sat_f64_s",
    "i64.trunc_sat_f64_u",
    "i64.xor",
    "if",
    "local.get",
    "local.set",
    "local.tee",
    "loop",
    "memory.copy",
    "memory.fill",
    "memory.grow",
    "memory.init",
    "memory.size",
    "nop",
    "ref.func",
    "ref.is_null",
    "ref.null",
    "return",
    "select",
    "table.copy",
    "table.fill",
    "table.get",
    "table.grow",
    "table.init",
    "table.set",
    "table.size",
    "unreachable",
    "v128.and",
    "v128.andnot",
    "v128.bitselect",
    "v128.const",
    "v128.load",
    "v128.not",
    "v128.or",
    "v128.store",
    "v128.xor",
];

/// Returns the first instruction that starts with `prefix`, if any,
/// and that prefix is not already a complete instruction.
///
/// Uses binary search — O(log n), safe to call on every keystroke.
///
/// `prefix` should be the text already typed on the current line
/// with leading whitespace stripped by the caller.
pub fn suggest(prefix: &str) -> Option<&'static str> {
    if prefix.is_empty() {
        return None;
    }
    let idx = WASM_INSTRS.partition_point(|&s| s < prefix);
    let candidate = WASM_INSTRS.get(idx)?;
    if candidate.starts_with(prefix) && *candidate != prefix {
        Some(candidate)
    } else {
        None
    }
}

pub fn suggest_all(prefix: &str) -> Vec<&'static str> {
    if prefix.is_empty() {
        return Vec::new();
    }
    let start_idx = WASM_INSTRS.partition_point(|&s| s < prefix);
    WASM_INSTRS[start_idx..]
        .iter()
        .take_while(|s| s.starts_with(prefix))
        .copied()
        .filter(|&s| s != prefix)
        .collect()
}

/// Given the full suggestion and the prefix already typed, returns only
/// the suffix that Tab should insert.
pub fn completion_suffix(full: &'static str, prefix: &str) -> &'static str {
    &full[prefix.trim_start().len()..]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_prefix() {
        assert_eq!(suggest("i32.co"), Some("i32.const"));
        assert_eq!(suggest("i32.const"), None); // exact match → no suggestion
        assert_eq!(suggest("f64."), Some("f64.abs")); // first alphabetically
        assert_eq!(suggest("local."), Some("local.get"));
        assert_eq!(suggest("mem"), Some("memory.copy"));
    }

    #[test]
    fn no_match() {
        assert_eq!(suggest(""), None);
        assert_eq!(suggest("xyz"), None);
        assert_eq!(suggest("drop"), None); // exact
    }

    #[test]
    fn suffix_only() {
        assert_eq!(completion_suffix("i32.const", "i32.co"), "nst");
        assert_eq!(completion_suffix("local.get", "local."), "get");
        assert_eq!(completion_suffix("memory.size", "  memory.si"), "ze"); // leading spaces
    }
}
