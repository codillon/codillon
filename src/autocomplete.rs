// Autocomplete functionality for WebAssembly instructions and module keywords

use std::collections::HashSet;

/// Represents a single autocomplete suggestion
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AutocompleteSuggestion {
    /// The text to display and insert
    pub text: String,
    /// Category of the suggestion (e.g., "instruction", "keyword", "control")
    pub category: SuggestionCategory,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SuggestionCategory {
    Instruction,
    Keyword,
    Control,
}

impl AutocompleteSuggestion {
    fn new(text: impl Into<String>, category: SuggestionCategory) -> Self {
        Self {
            text: text.into(),
            category,
        }
    }
}

/// Get all available autocomplete suggestions
pub fn get_all_suggestions() -> Vec<AutocompleteSuggestion> {
    let mut suggestions = HashSet::new();

    // Module structure keywords
    suggestions.insert(AutocompleteSuggestion::new("func", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("export", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("import", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("param", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("result", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("local", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("module", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("type", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("table", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("memory", SuggestionCategory::Keyword));
    suggestions.insert(AutocompleteSuggestion::new("global", SuggestionCategory::Keyword));

    // Control flow instructions
    suggestions.insert(AutocompleteSuggestion::new("block", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("loop", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("if", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("else", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("end", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("br", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("br_if", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("br_table", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("return", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("call", SuggestionCategory::Control));
    suggestions.insert(AutocompleteSuggestion::new("call_indirect", SuggestionCategory::Control));

    // Basic instructions
    suggestions.insert(AutocompleteSuggestion::new("nop", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("unreachable", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("drop", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("select", SuggestionCategory::Instruction));

    // i32 instructions
    suggestions.insert(AutocompleteSuggestion::new("i32.const", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.eqz", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.eq", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.ne", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.lt_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.lt_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.gt_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.gt_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.le_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.le_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.ge_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.ge_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.clz", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.ctz", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.popcnt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.add", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.sub", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.mul", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.div_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.div_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.rem_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.rem_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.and", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.or", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.xor", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.shl", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.shr_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.shr_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.rotl", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.rotr", SuggestionCategory::Instruction));

    // i64 instructions
    suggestions.insert(AutocompleteSuggestion::new("i64.const", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.eqz", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.eq", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.ne", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.lt_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.lt_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.gt_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.gt_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.le_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.le_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.ge_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.ge_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.clz", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.ctz", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.popcnt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.add", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.sub", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.mul", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.div_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.div_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.rem_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.rem_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.and", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.or", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.xor", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.shl", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.shr_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.shr_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.rotl", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.rotr", SuggestionCategory::Instruction));

    // f32 instructions
    suggestions.insert(AutocompleteSuggestion::new("f32.const", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.eq", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.ne", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.lt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.gt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.le", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.ge", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.abs", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.neg", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.ceil", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.floor", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.trunc", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.nearest", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.sqrt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.add", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.sub", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.mul", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.div", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.min", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.max", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.copysign", SuggestionCategory::Instruction));

    // f64 instructions
    suggestions.insert(AutocompleteSuggestion::new("f64.const", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.eq", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.ne", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.lt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.gt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.le", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.ge", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.abs", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.neg", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.ceil", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.floor", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.trunc", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.nearest", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.sqrt", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.add", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.sub", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.mul", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.div", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.min", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.max", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.copysign", SuggestionCategory::Instruction));

    // Conversion instructions
    suggestions.insert(AutocompleteSuggestion::new("i32.wrap_i64", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.trunc_f32_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.trunc_f32_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.trunc_f64_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.trunc_f64_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.extend_i32_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.extend_i32_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.trunc_f32_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.trunc_f32_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.trunc_f64_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.trunc_f64_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.convert_i32_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.convert_i32_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.convert_i64_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.convert_i64_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.demote_f64", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.convert_i32_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.convert_i32_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.convert_i64_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.convert_i64_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.promote_f32", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.reinterpret_f32", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.reinterpret_f64", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.reinterpret_i32", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.reinterpret_i64", SuggestionCategory::Instruction));

    // Local/Global variable instructions
    suggestions.insert(AutocompleteSuggestion::new("local.get", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("local.set", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("local.tee", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("global.get", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("global.set", SuggestionCategory::Instruction));

    // Memory instructions
    suggestions.insert(AutocompleteSuggestion::new("i32.load", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.load", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.load", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.load8_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.load8_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.load16_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.load16_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load8_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load8_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load16_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load16_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load32_s", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.load32_u", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.store", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.store", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f32.store", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("f64.store", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.store8", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i32.store16", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.store8", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.store16", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("i64.store32", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("memory.size", SuggestionCategory::Instruction));
    suggestions.insert(AutocompleteSuggestion::new("memory.grow", SuggestionCategory::Instruction));

    suggestions.into_iter().collect()
}

/// Get autocomplete suggestions filtered by a prefix
pub fn get_suggestions(prefix: &str) -> Vec<AutocompleteSuggestion> {
    let prefix_lower = prefix.to_lowercase();
    
    if prefix_lower.is_empty() {
        return get_all_suggestions();
    }

    get_all_suggestions()
        .into_iter()
        .filter(|suggestion| suggestion.text.to_lowercase().starts_with(&prefix_lower))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_all_suggestions_not_empty() {
        let suggestions = get_all_suggestions();
        assert!(!suggestions.is_empty());
        assert!(suggestions.len() > 100); // Should have many suggestions
    }

    #[test]
    fn test_get_suggestions_with_prefix() {
        let suggestions = get_suggestions("i32");
        assert!(!suggestions.is_empty());
        for suggestion in &suggestions {
            assert!(suggestion.text.to_lowercase().starts_with("i32"));
        }
    }

    #[test]
    fn test_get_suggestions_empty_prefix() {
        let all = get_all_suggestions();
        let filtered = get_suggestions("");
        assert_eq!(all.len(), filtered.len());
    }

    #[test]
    fn test_get_suggestions_case_insensitive() {
        let lower = get_suggestions("i32");
        let upper = get_suggestions("I32");
        assert_eq!(lower.len(), upper.len());
    }

    #[test]
    fn test_specific_instructions_exist() {
        let all = get_all_suggestions();
        let texts: Vec<_> = all.iter().map(|s| s.text.as_str()).collect();
        
        assert!(texts.contains(&"i32.add"));
        assert!(texts.contains(&"i32.const"));
        assert!(texts.contains(&"f64.mul"));
        assert!(texts.contains(&"block"));
        assert!(texts.contains(&"loop"));
        assert!(texts.contains(&"func"));
        assert!(texts.contains(&"local.get"));
    }
}
