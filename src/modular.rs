// Structures and utilities to support multi-function/multi-module.
//use crate::utils::InstrKind;
use wast::{
    core::{InlineExport, TypeUse, FunctionType, Local, LocalParser},
    parser::{Parse, Parser, Result},
    kw,
    token::{Id, NameAnnotation, Span},
};

/// The function header only, without closing parenthesis.
/// Containing expressions (instructions) within function header is 
/// considered as malformed. 
/// 
/// Modified from wasm-tools func.rs.
pub struct FuncHeader<'a> {
    /// Where this `func` was defined.
    pub span: Span,
    /// An identifier that this function is resolved with (optionally) for name
    /// resolution.
    pub id: Option<Id<'a>>,
    /// An optional name for this function stored in the custom `name` section.
    pub name: Option<NameAnnotation<'a>>,
    /// If present, inline export annotations which indicate names this
    /// definition should be exported under.
    pub exports: InlineExport<'a>,
    /// The type that this function will have.
    pub ty: TypeUse<'a, FunctionType<'a>>,
    /// Local variables declared like (local ...).
    pub locals: Box<[Local<'a>]>,
}

/// The function/module closing parenthesis.
pub struct FuncModuEnd {
    /// Where this closing was defined.
    pub span: Span,
}

impl<'a> Parse<'a> for FuncHeader<'a> {
    fn parse(parser: Parser<'a>) -> Result<Self> {
        let span = parser.parse::<kw::func>()?.0;
        let id = parser.parse()?;
        let name = parser.parse()?;
        let exports = parser.parse()?;
        let ty = parser.parse()?;

        let mut locals = Vec::new();
        while parser.peek2::<kw::local>()? {
            parser.parens(|p| {
                locals.extend(p.parse::<LocalParser>()?.locals);
                Ok(())
            })?;
        }

        Ok(FuncHeader {
            span,
            id,
            name,
            exports,
            ty,
            locals: locals.into(),
        })
    }
}
