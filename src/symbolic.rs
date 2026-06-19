// Symbolic reference utilities for producing valid syntax

use std::collections::HashSet;
use wast::{
    core::{
        ElemPayload, Export, ExportKind, Expression, Func, FunctionType, Global, GlobalKind,
        HeapType, ImportItems, Imports, InlineExport, Instruction, ItemKind, ItemSig, Local,
        LocalParser, Memory, Table, TableKind, ValType,
    },
    kw,
    parser::{self, Parse, ParseBuffer, Parser},
    token::{Id, Index},
};

use crate::syntax::{CodillonParam, CodillonResult, ImportKind, LineKind, ModulePart};

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

#[derive(Default)]
pub struct ModuleIdentifiers {
    // defined symbolic references in different identifier contexts
    // only module-level symbolic references are recorded
    types: HashSet<String>,
    globals: HashSet<String>,
    mems: HashSet<String>,
    tables: HashSet<String>,
    funcs: HashSet<String>,
    datas: HashSet<String>,
    elems: HashSet<String>,
}

impl ModuleIdentifiers {
    fn set(&self, space: &IndexSpace) -> Option<&HashSet<String>> {
        match space {
            IndexSpace::Type => Some(&self.types),
            IndexSpace::Global => Some(&self.globals),
            IndexSpace::Mem => Some(&self.mems),
            IndexSpace::Table => Some(&self.tables),
            IndexSpace::Func => Some(&self.funcs),
            IndexSpace::Data => Some(&self.datas),
            IndexSpace::Elem => Some(&self.elems),
            _ => None,
        }
    }

    fn set_mut(&mut self, space: &IndexSpace) -> Option<&mut HashSet<String>> {
        match space {
            IndexSpace::Type => Some(&mut self.types),
            IndexSpace::Global => Some(&mut self.globals),
            IndexSpace::Mem => Some(&mut self.mems),
            IndexSpace::Table => Some(&mut self.tables),
            IndexSpace::Func => Some(&mut self.funcs),
            IndexSpace::Data => Some(&mut self.datas),
            IndexSpace::Elem => Some(&mut self.elems),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Default, Debug, Clone)]
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

// Collect module-level defined symbols; no action for non-module-level symbols.
// Returns an error and revert the collection if any line symbol is a duplicate within
// its index space.
pub fn collect_module_symbols(
    line_symbols: &LineSymbols,
    identifiers: &mut ModuleIdentifiers,
) -> Result<(), &'static str> {
    let mut inserted: Vec<(&IndexSpace, &str)> = Vec::new();

    for symbol in &line_symbols.defines {
        if let Some(set) = identifiers.set_mut(&symbol.space) {
            if !set.insert(symbol.name.clone()) {
                // Revert collections and return with err
                for (space, name) in inserted {
                    identifiers.set_mut(space).unwrap().remove(name);
                }
                return Err("duplicate symbolic reference in same index space");
            }
            inserted.push((&symbol.space, &symbol.name));
        }
    }

    Ok(())
}

// Collect function-scoped defined Local symbols; no action for non-local symbols.
// Returns an error and revert the collection if any line symbol is a duplicate within
// that function scope.
pub fn collect_local_symbols(
    line_symbols: &LineSymbols,
    locals: &mut HashSet<String>,
) -> Result<(), &'static str> {
    let mut inserted: Vec<&str> = Vec::new();

    for symbol in &line_symbols.defines {
        if symbol.space != IndexSpace::Local {
            continue;
        }
        if !locals.insert(symbol.name.clone()) {
            // Revert collections and return with err
            for name in inserted {
                locals.remove(name);
            }
            return Err("duplicate local symbolic reference");
        }
        inserted.push(&symbol.name);
    }

    Ok(())
}

// Return the label defined in the line, if there's any, ignoring non-label symbols.
pub fn collect_label_symbol(line_symbols: &LineSymbols) -> Option<String> {
    assert!(line_symbols.defines.len() <= 1);

    if line_symbols.defines.is_empty() {
        None
    } else {
        Some(line_symbols.defines[0].name.clone())
    }
}

/// Return true if all consumed symbolic references in are resolved:
/// - module-level symbols (funcs, globals, etc.) are looked up in ModuleIdentifiers
/// - locals are looked up in the function-scoped locals set
/// - labels are looked up in the current label stack
pub fn symbols_resolved(
    line_symbols: &LineSymbols,
    identifiers: &ModuleIdentifiers,
    locals: &HashSet<String>,
    label_stack: &[String],
) -> bool {
    line_symbols.consumes.iter().all(|sym| match sym.space {
        IndexSpace::Local => locals.contains(&sym.name),
        IndexSpace::Label => label_stack.contains(&sym.name),
        _ => identifiers
            .set(&sym.space)
            .is_some_and(|set| set.contains(&sym.name)),
    })
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

            Instruction::BrTable(i) => {
                for label in &i.labels {
                    if let Index::Id(id) = label {
                        consumes.push(SymbolRef::new(id.name(), IndexSpace::Label));
                    }
                }
                if let Index::Id(id) = i.default {
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
            | Instruction::I32Load8s(i)
            | Instruction::I32Load8u(i)
            | Instruction::I32Load16s(i)
            | Instruction::I32Load16u(i)
            | Instruction::I64Load(i)
            | Instruction::I64Load8s(i)
            | Instruction::I64Load8u(i)
            | Instruction::I64Load16s(i)
            | Instruction::I64Load16u(i)
            | Instruction::I64Load32s(i)
            | Instruction::I64Load32u(i)
            | Instruction::F32Load(i)
            | Instruction::F64Load(i)
            | Instruction::I32Store(i)
            | Instruction::I32Store8(i)
            | Instruction::I32Store16(i)
            | Instruction::I64Store(i)
            | Instruction::I64Store8(i)
            | Instruction::I64Store16(i)
            | Instruction::I64Store32(i)
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

impl<'a> From<LocalParser<'a>> for LineSymbols {
    fn from(p: LocalParser) -> Self {
        let mut line_symbols = LineSymbols::default();

        for Local { id, ty, .. } in p.locals {
            if let Some(id) = id {
                line_symbols
                    .defines
                    .push(SymbolRef::new(id.name(), IndexSpace::Local));
            }

            line_symbols.merge(ty.into());
        }

        line_symbols
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

struct Parened<T>(T);

impl<'a, T: Parse<'a>> Parse<'a> for Parened<T> {
    fn parse(parser: Parser<'a>) -> Result<Self, wast::Error> {
        Ok(Self(parser.parens(T::parse)?))
    }
}

impl<T: Into<LineSymbols>> From<Parened<T>> for LineSymbols {
    fn from(val: Parened<T>) -> Self {
        val.0.into()
    }
}

struct FuncDefs(LineSymbols);
impl<'a> Parse<'a> for FuncDefs {
    fn parse(parser: Parser<'a>) -> Result<Self, wast::Error> {
        let mut syms = LineSymbols::default();
        while !parser.is_empty() || parser.step(|c| Ok((c.peek_rparen()?, c)))? {
            if parser.peek::<kw::func>()? {
                parser.parse::<kw::func>()?;
            } else if parser.peek::<Id>()? {
                syms.defines.push(SymbolRef::new(
                    parser.parse::<Id>()?.name(),
                    IndexSpace::Func,
                ));
            } else if parser.peek::<InlineExport>()? {
                parser.parse::<InlineExport>()?;
            } else if parser.peek2::<kw::param>()? {
                syms.merge(parser.parse::<CodillonParam>()?.0.into());
            } else if parser.peek2::<kw::result>()? {
                syms.merge(parser.parse::<CodillonResult>()?.0.into());
            } else if parser.peek2::<kw::local>()? {
                syms.merge(parser.parse::<Parened<LocalParser>>()?.into());
            } else if parser.step(|cursor| match cursor.lparen()? {
                Some(rest) => Ok((true, rest)),
                None => Ok((false, cursor)),
            })? || parser.step(|cursor| match cursor.rparen()? {
                Some(rest) => Ok((true, rest)),
                None => Ok((false, cursor)),
            })? {
            } else {
                panic!("unexpected token")
            }
        }
        Ok(Self(syms))
    }
}

// Assume kind correctly reflects the line's kind
pub fn parse_line_symbols(s: &str, kind: &LineKind) -> LineSymbols {
    let buf = ParseBuffer::new(s).unwrap();
    match kind {
        LineKind::Instr(_) => parser::parse::<Instruction>(&buf)
            .map(Into::into)
            .unwrap_or_default(),
        LineKind::Other(part) => match part {
            ModulePart::Export => parser::parse::<Parened<Export>>(&buf).unwrap().into(),
            ModulePart::Import(kind) => match kind {
                ImportKind::Import => parser::parse::<Parened<Imports>>(&buf).unwrap().into(),
                ImportKind::InlineFunc => {
                    if let Some(name) = parser::parse::<Parened<Func>>(&buf).unwrap().0.id {
                        LineSymbols {
                            defines: vec![SymbolRef::new(name.name(), IndexSpace::Func)],
                            consumes: vec![],
                        }
                    } else {
                        LineSymbols::default()
                    }
                }
                ImportKind::InlineMemory => parser::parse::<Parened<Memory>>(&buf).unwrap().into(),
                ImportKind::InlineGlobal => parser::parse::<Parened<Global>>(&buf).unwrap().into(),
            },
            ModulePart::Global => parser::parse::<Parened<Global>>(&buf).unwrap().into(),
            ModulePart::Table => parser::parse::<Parened<Table>>(&buf).unwrap().into(),
            ModulePart::Memory => parser::parse::<Parened<Memory>>(&buf).unwrap().into(),
            ModulePart::Func(_) => parser::parse::<FuncDefs>(&buf).unwrap().0,
        },
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
        let mut symbols = parse_line_symbols(line, &LineKind::Empty);
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "block $label";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::OtherStructured));
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "label");
        assert_eq!(symbols.defines[0].space, IndexSpace::Label);
        assert!(symbols.consumes.is_empty());

        line = "block $label (type $x) (param i32)";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::OtherStructured));
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "label");
        assert_eq!(symbols.defines[0].space, IndexSpace::Label);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "x");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = " br $label";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "label");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Label);

        line = "br 1";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "br_table $a $b $default";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 3);
        assert_eq!(symbols.consumes[0].name, "a");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Label);
        assert_eq!(symbols.consumes[1].name, "b");
        assert_eq!(symbols.consumes[1].space, IndexSpace::Label);
        assert_eq!(symbols.consumes[2].name, "default");
        assert_eq!(symbols.consumes[2].space, IndexSpace::Label);

        line = "local.set $something";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "something");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Local);

        line = "memory.copy $x $y";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 2);
        assert_eq!(symbols.consumes[0].name, "x");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Mem);
        assert_eq!(symbols.consumes[1].name, "y");
        assert_eq!(symbols.consumes[1].space, IndexSpace::Mem);

        line = "memory.copy 10 $y";
        symbols = parse_line_symbols(line, &LineKind::Instr(InstrKind::Other));
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "y");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Mem);

        line = "(global i32 (i32.const 0))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert!(symbols.consumes.is_empty());

        line = "(global $x i32 (i32.const 0))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "x");
        assert_eq!(symbols.defines[0].space, IndexSpace::Global);
        assert!(symbols.consumes.is_empty());

        /* disable test now that global initialization is syntactically restricted
                line = "(global $x i32 (global.get $y))";
                symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
                assert_eq!(symbols.defines.len(), 1);
                assert_eq!(symbols.defines[0].name, "x");
                assert_eq!(symbols.defines[0].space, IndexSpace::Global);
                assert_eq!(symbols.consumes.len(), 1);
                assert_eq!(symbols.consumes[0].name, "y");
                assert_eq!(symbols.consumes[0].space, IndexSpace::Global);
        */

        line = "(table $t 1 (ref $ft))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "t");
        assert_eq!(symbols.defines[0].space, IndexSpace::Table);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "ft");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = "(table funcref (elem $f1 $f2))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 2);
        assert_eq!(symbols.consumes[0].name, "f1");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Func);
        assert_eq!(symbols.consumes[1].name, "f2");
        assert_eq!(symbols.consumes[1].space, IndexSpace::Func);

        line = "(memory $mem 1 2)";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 1);
        assert_eq!(symbols.defines[0].name, "mem");
        assert_eq!(symbols.defines[0].space, IndexSpace::Mem);
        assert!(symbols.consumes.is_empty());

        line = "(export \"foo\" (func $myfunc))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert!(symbols.defines.is_empty());
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "myfunc");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Func);

        line = "(import \"draw\" \"set_radius\"  (func $f (param $p f64)))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 2);
        assert_eq!(symbols.defines[0].name, "f");
        assert_eq!(symbols.defines[0].space, IndexSpace::Func);
        assert_eq!(symbols.defines[1].name, "p");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert!(symbols.consumes.is_empty());

        /* type index not supported
                line = "(import \"env\" \"fn\"  (func $f (type $t)))";
                symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
                assert_eq!(symbols.defines.len(), 1);
                assert_eq!(symbols.defines[0].name, "f");
                assert_eq!(symbols.defines[0].space, IndexSpace::Func);
                assert_eq!(symbols.consumes.len(), 1);
                assert_eq!(symbols.consumes[0].name, "t");
                assert_eq!(symbols.consumes[0].space, IndexSpace::Type);
        */

        line = "(func $f (param $p i32) (result (ref $ft)) (local $l i64))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
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
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 2);
        assert_eq!(symbols.defines[0].name, "l1");
        assert_eq!(symbols.defines[0].space, IndexSpace::Local);
        assert_eq!(symbols.defines[1].name, "l2");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert_eq!(symbols.consumes.len(), 1);
        assert_eq!(symbols.consumes[0].name, "ft");
        assert_eq!(symbols.consumes[0].space, IndexSpace::Type);

        line = "  (export \"\") (param $x i32) (param $y i64))";
        symbols = parse_line_symbols(line, &parse::<LineKind>(&ParseBuffer::new(line)?)?);
        assert_eq!(symbols.defines.len(), 2);
        assert_eq!(symbols.defines[0].name, "x");
        assert_eq!(symbols.defines[0].space, IndexSpace::Local);
        assert_eq!(symbols.defines[1].name, "y");
        assert_eq!(symbols.defines[1].space, IndexSpace::Local);
        assert!(symbols.consumes.is_empty());

        line = "(import \"\" \"\") (local $l f32)";
        assert!(parse::<LineKind>(&ParseBuffer::new(line)?).is_err());

        Ok(())
    }
}
