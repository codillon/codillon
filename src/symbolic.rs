// Symbolic reference utilities for producing valid syntax

use std::collections::HashSet;
use wast::{
    core::{
        ElemPayload, Export, ExportKind, Expression, Func, FuncKind, FunctionType, Global,
        GlobalKind, HeapType, ImportItems, Imports, Instruction, ItemKind, ItemSig, Local, Memory,
        ModuleField, Table, TableKind, ValType,
    },
    parser::{self, ParseBuffer},
    token::Index,
};

use crate::syntax::{LineKind, ModulePart};

#[derive(Clone, Debug, PartialEq)]
enum IndexSpace {
    Type,
    Global,
    Mem,
    Table,
    Func,
    Data,
    Elem,
    Local,
    Label,
    Undefined,
}

impl From<ExportKind> for IndexSpace {
    fn from(kind: ExportKind) -> Self {
        match kind {
            ExportKind::Func => IndexSpace::Func,
            ExportKind::Table => IndexSpace::Table,
            ExportKind::Memory => IndexSpace::Mem,
            ExportKind::Global => IndexSpace::Global,
            _ => IndexSpace::Undefined,
        }
    }
}

impl From<&ItemKind<'_>> for IndexSpace {
    fn from(kind: &ItemKind<'_>) -> Self {
        match kind {
            ItemKind::Func(_) | ItemKind::FuncExact(_) => IndexSpace::Func,
            ItemKind::Table(_) => IndexSpace::Table,
            ItemKind::Memory(_) => IndexSpace::Mem,
            ItemKind::Global(_) => IndexSpace::Global,
            _ => IndexSpace::Undefined,
        }
    }
}

pub struct Identifiers {
    // defined symbolic references in different indentifier contexts
    types: HashSet<String>,
    globals: HashSet<String>,
    mems: HashSet<String>,
    tables: HashSet<String>,
    funcs: HashSet<String>,
    datas: HashSet<String>,
    elems: HashSet<String>,
    locals: HashSet<String>,
    labels: HashSet<String>,
}

impl Identifiers {
    pub fn reset(&mut self) {
        self.types.clear();
        self.globals.clear();
        self.mems.clear();
        self.tables.clear();
        self.funcs.clear();
        self.datas.clear();
        self.elems.clear();
        self.locals.clear();
        self.labels.clear();
    }

    pub fn push_symbols(&mut self, symbols: Vec<SymbolRef>) {
        for symbol in symbols {
            let set = match symbol.space {
                IndexSpace::Type => &mut self.types,
                IndexSpace::Global => &mut self.globals,
                IndexSpace::Mem => &mut self.mems,
                IndexSpace::Table => &mut self.tables,
                IndexSpace::Func => &mut self.funcs,
                IndexSpace::Data => &mut self.datas,
                IndexSpace::Elem => &mut self.elems,
                IndexSpace::Local => &mut self.locals,
                IndexSpace::Label => &mut self.labels,
                IndexSpace::Undefined => continue,
            };
            set.insert(symbol.name);
        }
    }
}

#[derive(Clone)]
pub struct SymbolRef {
    name: String,
    space: IndexSpace,
}

impl SymbolRef {
    fn new(name: &str, space: IndexSpace) -> Self {
        Self {
            name: name.to_string(),
            space,
        }
    }
}

#[derive(Default, Clone)]
pub struct LineSymbols {
    defines: Vec<SymbolRef>,
    consumes: Vec<SymbolRef>,
}

impl LineSymbols {
    fn merge(&mut self, other: LineSymbols) {
        self.defines.extend(other.defines);
        self.consumes.extend(other.consumes);
    }
}

impl From<Instruction<'_>> for LineSymbols {
    fn from(instr: Instruction<'_>) -> Self {
        // Defining keywords for instructions: block, loop, if
        // Not all instructions are included for symbol collection.
        // They may be added incrementally
        let mut defines: Vec<SymbolRef> = Vec::new();
        let mut consumes: Vec<SymbolRef> = Vec::new();

        match instr {
            // Variant(Box<BlockType<'a>>)
            Instruction::Block(i) | Instruction::Loop(i) | Instruction::If(i) => {
                if let Some(id) = i.label {
                    defines.push(SymbolRef::new(id.name(), IndexSpace::Label));
                }

                // These instructions can involve (type $id), e.g. "block (type $x)"
                if let Some(Index::Id(id)) = i.ty.index {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Type));
                }
            }

            // Variant(Index<'a>)
            Instruction::Br(i) | Instruction::BrIf(i) => {
                if let Index::Id(id) = i {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Label));
                }
            }

            Instruction::Call(i)
            | Instruction::ReturnCall(i)
            | Instruction::CallRef(i)
            | Instruction::ReturnCallRef(i) => {
                if let Index::Id(id) = i {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Func));
                }
            }

            Instruction::LocalGet(i) | Instruction::LocalSet(i) | Instruction::LocalTee(i) => {
                if let Index::Id(id) = i {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Local));
                }
            }

            Instruction::GlobalGet(i) | Instruction::GlobalSet(i) => {
                if let Index::Id(id) = i {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Global));
                }
            }

            Instruction::StructNew(i)
            | Instruction::StructNewDefault(i)
            | Instruction::ArrayNew(i)
            | Instruction::ArrayNewDefault(i)
            | Instruction::ArrayGet(i)
            | Instruction::ArraySet(i) => {
                if let Index::Id(id) = i {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Type));
                }
            }

            // Variant(Option<Id<'a>>)
            Instruction::Else(i) | Instruction::End(i) => {
                if let Some(id) = i {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Label));
                }
            }

            // Variant(TableArg<'a>)
            Instruction::TableGet(i)
            | Instruction::TableSet(i)
            | Instruction::TableFill(i)
            | Instruction::TableSize(i)
            | Instruction::TableGrow(i) => {
                if let Index::Id(id) = i.dst {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Table));
                }
            }

            // Variant(MemArg<'a>)
            Instruction::I32Load(i)
            | Instruction::I64Load(i)
            | Instruction::F32Load(i)
            | Instruction::F64Load(i)
            | Instruction::I32Store(i)
            | Instruction::I64Store(i)
            | Instruction::F32Store(i)
            | Instruction::F64Store(i) => {
                if let Index::Id(id) = i.memory {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Mem));
                }
            }

            // Variant(MemoryArg<'a>)
            Instruction::MemorySize(i)
            | Instruction::MemoryGrow(i)
            | Instruction::MemoryFill(i)
            | Instruction::MemoryDiscard(i) => {
                if let Index::Id(id) = i.mem {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Mem));
                }
            }

            // Special variants
            Instruction::MemoryInit(i) => {
                if let Index::Id(id) = i.data {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Data));
                }
                if let Index::Id(id) = i.mem {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Mem));
                }
            }

            Instruction::MemoryCopy(i) => {
                if let Index::Id(id) = i.dst {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Mem));
                }
                if let Index::Id(id) = i.src {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Mem));
                }
            }

            Instruction::TableInit(i) => {
                if let Index::Id(id) = i.table {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Table));
                }
                if let Index::Id(id) = i.elem {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Elem));
                }
            }

            Instruction::TableCopy(i) => {
                if let Index::Id(id) = i.dst {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Table));
                }
                if let Index::Id(id) = i.src {
                    consumes.push(SymbolRef::new(id.name(), IndexSpace::Table));
                }
            }

            _ => {}
        }

        LineSymbols { defines, consumes }
    }
}

impl<'a> From<Expression<'a>> for LineSymbols {
    fn from(expr: Expression) -> Self {
        let mut line_symbols = LineSymbols::default();

        for instr in expr.instrs.iter() {
            line_symbols.merge(instr.clone().into());
        }

        line_symbols
    }
}

impl<'a> From<HeapType<'a>> for LineSymbols {
    fn from(ht: HeapType) -> Self {
        match ht {
            HeapType::Concrete(Index::Id(id)) | HeapType::Exact(Index::Id(id)) => Self {
                defines: Vec::new(),
                consumes: vec![SymbolRef::new(id.name(), IndexSpace::Type)],
            },
            _ => Self::default(),
        }
    }
}

impl<'a> From<ValType<'a>> for LineSymbols {
    fn from(vt: ValType) -> Self {
        match vt {
            ValType::Ref(rt) => rt.heap.into(),
            _ => Self::default(),
        }
    }
}

impl<'a> From<FunctionType<'a>> for LineSymbols {
    fn from(ft: FunctionType) -> Self {
        let mut line_symbols = LineSymbols::default();

        for (id, _, vt) in ft.params.iter() {
            if let Some(id) = id {
                line_symbols
                    .defines
                    .push(SymbolRef::new(id.name(), IndexSpace::Local));
            };
            line_symbols.merge((*vt).into());
        }

        for vt in ft.results.iter() {
            line_symbols.merge((*vt).into());
        }

        line_symbols
    }
}

impl<'a> From<ItemSig<'a>> for LineSymbols {
    fn from(item: ItemSig) -> Self {
        let mut line_symbols = LineSymbols::default();

        if let Some(id) = item.id {
            line_symbols
                .defines
                .push(SymbolRef::new(id.name(), (&item.kind).into()));
        }

        match item.kind {
            ItemKind::Func(f) | ItemKind::FuncExact(f) => {
                if let Some(Index::Id(id)) = f.index {
                    line_symbols
                        .consumes
                        .push(SymbolRef::new(id.name(), IndexSpace::Type));
                }
                if let Some(ft) = f.inline {
                    line_symbols.merge(ft.into());
                }
            }
            ItemKind::Table(t) => {
                line_symbols.merge(t.elem.heap.into());
            }
            ItemKind::Global(g) => {
                line_symbols.merge(g.ty.into());
            }
            _ => {}
        }

        line_symbols
    }
}

impl<'a> From<Export<'a>> for LineSymbols {
    fn from(export: Export) -> Self {
        match export.item {
            Index::Id(id) => Self {
                defines: Vec::new(),
                consumes: vec![SymbolRef::new(id.name(), export.kind.into())],
            },
            _ => Self::default(),
        }
    }
}

impl<'a> From<Imports<'a>> for LineSymbols {
    fn from(import: Imports) -> Self {
        match import.items {
            ImportItems::Single { sig, .. } => sig.into(),
            ImportItems::Group1 { .. } => Self::default(),
            ImportItems::Group2 { sig, .. } => sig.into(),
        }
    }
}

impl<'a> From<Local<'a>> for LineSymbols {
    fn from(local: Local) -> Self {
        let mut line_symbols = LineSymbols::default();

        if let Some(id) = local.id {
            line_symbols
                .defines
                .push(SymbolRef::new(id.name(), IndexSpace::Local));
        };

        line_symbols.merge(local.ty.into());

        line_symbols
    }
}

impl<'a> From<Global<'a>> for LineSymbols {
    fn from(global: Global) -> Self {
        let mut line_symbols = LineSymbols::default();

        if let Some(id) = global.id {
            line_symbols
                .defines
                .push(SymbolRef::new(id.name(), IndexSpace::Global));
        };

        if let GlobalKind::Inline(expr) = global.kind {
            line_symbols.merge(expr.into());
        };

        line_symbols
    }
}

impl<'a> From<Table<'a>> for LineSymbols {
    fn from(table: Table) -> Self {
        let mut line_symbols = LineSymbols::default();

        if let Some(id) = table.id {
            line_symbols
                .defines
                .push(SymbolRef::new(id.name(), IndexSpace::Table));
        };

        match table.kind {
            TableKind::Import { ty, .. } => {
                line_symbols.merge(ty.elem.heap.into());
            }
            TableKind::Normal { ty, init_expr } => {
                line_symbols.merge(ty.elem.heap.into());
                if let Some(expr) = init_expr {
                    for instr in expr.instrs.iter() {
                        line_symbols.merge(instr.clone().into());
                    }
                };
            }
            TableKind::Inline { elem, payload, .. } => {
                line_symbols.merge(elem.heap.into());
                match payload {
                    ElemPayload::Indices(indices) => {
                        line_symbols.consumes.extend(indices.iter().filter_map(
                            |index| match index {
                                Index::Id(id) => Some(SymbolRef::new(id.name(), IndexSpace::Func)),
                                _ => None,
                            },
                        ));
                    }
                    ElemPayload::Exprs { ty, exprs } => {
                        line_symbols.merge(ty.heap.into());
                        for expr in exprs {
                            line_symbols.merge(expr.into());
                        }
                    }
                }
            }
        };

        line_symbols
    }
}

impl<'a> From<Memory<'a>> for LineSymbols {
    fn from(memory: Memory) -> Self {
        match memory.id {
            Some(id) => Self {
                defines: vec![SymbolRef::new(id.name(), IndexSpace::Mem)],
                consumes: Vec::new(),
            },
            None => Self::default(),
        }
    }
}

// Assume kind correctly reflects the line's kind
pub fn parse_line_symbols(s: &str, kind: LineKind) -> LineSymbols {
    match kind {
        LineKind::Instr(_) => {
            let Ok(buf) = ParseBuffer::new(s) else {
                return LineSymbols::default();
            };
            parser::parse::<Instruction>(&buf)
                .map(Into::into)
                .unwrap_or_default()
        }
        LineKind::Other(module_parts) if !module_parts.is_empty() && s.len() > 1 => {
            // Line is non-function module part
            let Ok(buf) = ParseBuffer::new(&s[1..s.len() - 1]) else {
                return LineSymbols::default();
            };
            if let Ok(field) = parser::parse::<ModuleField>(&buf) {
                match field {
                    ModuleField::Export(e) => return e.into(),
                    ModuleField::Import(i) => return i.into(),
                    ModuleField::Global(g) => return g.into(),
                    ModuleField::Table(t) => return t.into(),
                    ModuleField::Memory(m) => return m.into(),
                    _ => {}
                }
            }

            // Line is made up of function segments
            let mut wrapped_s = match module_parts[0] {
                ModulePart::FuncKeyword => s.to_string(),
                ModulePart::LParen => s[1..].to_string(),
                _ => format!("func {s}"),
            };
            if matches!(module_parts.last(), Some(ModulePart::RParen)) {
                wrapped_s = wrapped_s
                    .strip_suffix(')')
                    .unwrap_or(&wrapped_s)
                    .to_string();
            }

            let Ok(wrapped_buf) = ParseBuffer::new(&wrapped_s) else {
                return LineSymbols::default();
            };
            let Ok(func) = parser::parse::<Func>(&wrapped_buf) else {
                return LineSymbols::default();
            };

            let mut line_symbols = LineSymbols::default();
            // Id
            if let Some(id) = func.id {
                line_symbols
                    .defines
                    .push(SymbolRef::new(id.name(), IndexSpace::Func));
            }
            // Param & Result
            if let Some(ft) = func.ty.inline {
                line_symbols.merge(ft.into());
            }
            // Local
            if let FuncKind::Inline { locals, .. } = func.kind {
                for local in locals {
                    line_symbols.merge(local.into());
                }
            }
            line_symbols
        }
        _ => LineSymbols::default(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::InstrKind;
    use anyhow::Result;
    use wast::parser::parse;

    #[test]
    fn test_parse_line_symbols() -> Result<()> {
        let mut line = "";
        let mut symbols = parse_line_symbols(line, LineKind::Empty);
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "block $label";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::OtherStructured));
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "label");
        assert_eq!(symbols.defines[0].space, IndexSpace::Label);
        assert!(symbols.consumes.is_empty());

        line = "block $label (type $x) (param i32)";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::OtherStructured));
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "label");
        assert_eq!(symbols.defines[0].space, IndexSpace::Label);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "x");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = " br $label";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "label");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Label);

        line = "br 1";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "local.set $something";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "something");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Local);

        line = "memory.copy $x $y";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 2);
        assert_eq!(symbols.consumes[0].name, "x");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Mem);
        assert_eq!(symbols.consumes[1].name, "y");
        assert_eq!(symbols.consumes[1].space, IndexSpace::Mem);

        line = "memory.copy 10 $y";
        symbols = parse_line_symbols(line, LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "y");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Mem);

        line = "(global i32 (i32.const 0))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "(global $x i32 (i32.const 0))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "x");
        assert_eq!(symbols.defines[0].space, IndexSpace::Global);
        assert!(symbols.consumes.is_empty());

        line = "(global $x i32 (global.get $y))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "x");
        assert_eq!(symbols.defines[0].space, IndexSpace::Global);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "y");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Global);

        line = "(table $t 1 (ref $ft))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "t");
        assert_eq!(symbols.defines[0].space, IndexSpace::Table);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "ft");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = "(table funcref (elem $f1 $f2))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 2);
        assert_eq!(symbols.consumes[0].name, "f1");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Func);
        assert_eq!(symbols.consumes[1].name, "f2");
        assert_eq!(symbols.consumes[1].space, IndexSpace::Func);

        line = "(memory $mem 1 2)";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "mem");
        assert_eq!(symbols.defines[0].space, IndexSpace::Mem);
        assert!(symbols.consumes.is_empty());

        line = "(memory $mem 1 2) (func)";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "(export \"foo\" (func $myfunc))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "myfunc");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Func);

        line = "(import \"env\" \"fn\"  (func $f (param $p i32)))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 2);
        assert_eq!(symbols.defines[0].name, "f");
        assert_eq!(symbols.defines[0].space, IndexSpace::Func);
        assert_eq!(symbols.defines[1].name, "p");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert!(symbols.consumes.is_empty());

        line = "(import \"env\" \"fn\"  (func $f (type $t)))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "f");
        assert_eq!(symbols.defines[0].space, IndexSpace::Func);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "t");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = "(func $f (param $p i32) (result (ref $ft)) (local $l i64))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 3);
        assert_eq!(symbols.defines[0].name, "f");
        assert_eq!(symbols.defines[0].space, IndexSpace::Func);
        assert_eq!(symbols.defines[1].name, "p");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert_eq!(symbols.defines[2].name, "l");
        assert_eq!(symbols.defines[2].space, IndexSpace::Local);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "ft");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = " func  (local $l1 i64) (local $l2 (ref $ft))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 2);
        assert_eq!(symbols.defines[0].name, "l1");
        assert_eq!(symbols.defines[0].space, IndexSpace::Local);
        assert_eq!(symbols.defines[1].name, "l2");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "ft");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = "  (export \"\") (param $x i32) (param $y i64))";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 2);
        assert_eq!(symbols.defines[0].name, "x");
        assert_eq!(symbols.defines[0].space, IndexSpace::Local);
        assert_eq!(symbols.defines[1].name, "y");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert!(symbols.consumes.is_empty());

        line = "(import \"\" \"\") (local $l f32)";
        symbols = parse_line_symbols(line, parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        Ok(())
    }
}
