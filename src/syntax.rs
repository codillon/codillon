// Structures and utilities to support multi-function/multi-module text buffers.

use wast::{
    Error,
    core::Instruction,
    kw,
    parser::{Cursor, Parse, Parser, Peek},
};

use anyhow::Result;

#[cfg(test)]
use wast::parser::{ParseBuffer, parse};

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum InstrKind {
    If,
    Else,
    End,
    OtherStructured, // block, loop, or try_table
    Other,           // any other instruction
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ModulePart {
    LParen,
    FuncBegin,
    RParen,
}

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub enum LineKind {
    #[default]
    Empty,
    Instr(InstrKind),
    Other(Vec<ModulePart>),
    Malformed(String), // explanation
}

impl From<Instruction<'_>> for InstrKind {
    fn from(instr: Instruction<'_>) -> Self {
        match instr {
            Instruction::If(_) => InstrKind::If,
            Instruction::Else(_) => InstrKind::Else,
            Instruction::End(_) => InstrKind::End,
            Instruction::Block(_) | Instruction::Loop(_) | Instruction::TryTable(_) => {
                InstrKind::OtherStructured
            }
            Instruction::Try(_) | Instruction::Catch(_) | Instruction::CatchAll => {
                panic!("legacy-exceptions not supported");
            }
            _ => InstrKind::Other,
        }
    }
}

impl From<Error> for LineKind {
    fn from(e: Error) -> Self {
        LineKind::Malformed(format!("{e}").lines().next().unwrap_or_default().into())
    }
}

impl<'a> Parse<'a> for ModulePart {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        if parser.step(|cursor| match cursor.lparen()? {
            Some(rest) => Ok((true, rest)),
            None => Ok((false, cursor)),
        })? {
            Ok(ModulePart::LParen)
        } else if parser.step(|cursor| match cursor.rparen()? {
            Some(rest) => Ok((true, rest)),
            None => Ok((false, cursor)),
        })? {
            Ok(ModulePart::RParen)
        } else if parser.parse::<kw::func>().is_ok() {
            Ok(ModulePart::FuncBegin)
        } else {
            Err(parser.error("includes instruction or malformed syntax"))
        }
    }
}

impl Peek for ModulePart {
    fn peek(cursor: Cursor) -> Result<bool, Error> {
        if cursor.peek_lparen()? || cursor.peek_rparen()? || kw::func::peek(cursor)? {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn display() -> &'static str {
        "'(' or ')' or 'func'"
    }
}

#[test]
fn test_parse_modulepart() -> Result<()> {
    assert_eq!(
        parse::<ModulePart>(&ParseBuffer::new("    (   ")?)?,
        ModulePart::LParen
    );
    assert_eq!(
        parse::<ModulePart>(&ParseBuffer::new("    )    ")?)?,
        ModulePart::RParen
    );
    assert_eq!(
        parse::<ModulePart>(&ParseBuffer::new("    func    ")?)?,
        ModulePart::FuncBegin
    );
    assert!(parse::<ModulePart>(&ParseBuffer::new("()")?).is_err());
    Ok(())
}

impl<'a> Parse<'a> for LineKind {
    fn parse(parser: Parser<'a>) -> Result<Self, Error> {
        // In the Codillon editor, a line can contain a single instruction
        // or a (possibly empty) sequence of ModuleParts.

        if parser.is_empty() && !parser.peek::<ModulePart>()? {
            Ok(LineKind::Empty)
        } else if parser.peek::<ModulePart>()? {
            let mut parts = Vec::new();
            while (!parser.is_empty()) || parser.peek::<ModulePart>()? {
                parts.push(parser.parse()?);
            }
            Ok(LineKind::Other(parts))
        } else {
            Ok(LineKind::Instr(parser.parse::<Instruction>()?.into()))
        }
    }
}

#[test]
fn test_parse_linekind() -> Result<()> {
    assert_eq!(parse::<LineKind>(&ParseBuffer::new("")?)?, LineKind::Empty);

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("     ")?)?,
        LineKind::Empty
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("i32.const 4")?)?,
        LineKind::Instr(InstrKind::Other)
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("   i32.const 4   ")?)?,
        LineKind::Instr(InstrKind::Other)
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("   if   ")?)?,
        LineKind::Instr(InstrKind::If)
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("   (   ")?)?,
        LineKind::Other(vec![ModulePart::LParen])
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new(")")?)?,
        LineKind::Other(vec![ModulePart::RParen])
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("()")?)?,
        LineKind::Other(vec![ModulePart::LParen, ModulePart::RParen])
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new(")func")?)?,
        LineKind::Other(vec![ModulePart::RParen, ModulePart::FuncBegin])
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new(") func")?)?,
        LineKind::Other(vec![ModulePart::RParen, ModulePart::FuncBegin])
    );

    assert_eq!(
        parse::<LineKind>(&ParseBuffer::new("   (func)   ")?)?,
        LineKind::Other(vec![
            ModulePart::LParen,
            ModulePart::FuncBegin,
            ModulePart::RParen
        ])
    );

    assert!(parse::<LineKind>(&ParseBuffer::new(") func i32.const 7")?).is_err());

    Ok(())
}
