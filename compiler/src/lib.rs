#![feature(map_try_insert)]
#![feature(iter_intersperse)]

use crate::analysis::decl::DeclQuery;
use crate::database::CompilerDatabase;
use crate::error_reporter::{
    new_print_errors, print_cannot_find_primitive_decl, DummyReportValues,
};
use crate::language::lexer::Token;
use crate::loader::WorkshopScriptLoader;
use crate::printer::PrinterQuery;
use crate::source_cache::{LookupSourceCache, SourceCache};
use crate::span::{CopyRange, SpanInterner, StringInterner};
use chumsky::error::Rich;
use chumsky::input::Input;
use chumsky::input::Stream as ChumskyStream;
use chumsky::Parser;
use error::CompilerError;
use hashlink::LinkedHashMap;
use salsa::Durability;
use smallvec::SmallVec;
use span::Span;
use span::StringId;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::path::PathBuf;
use trisult::Trisult;

pub mod analysis;
pub mod cir;
// pub mod codegen;
pub mod codegen;
pub mod cst;
pub mod database;
pub mod error;
pub mod error_reporter;
pub mod language;
pub mod loader;
pub mod printer;
pub mod source_cache;
pub mod span;
pub mod test_assert;
pub mod trisult;
pub mod wir;
pub mod workshop;
pub mod wst;

pub type FullText = String;
pub type TextId = StringId;

pub type HashableMap<K, V> = LinkedHashMap<K, V>;
pub type QueryTrisult<T> = Trisult<T, CompilerError>;

pub const CONDITIONS_LEN: usize = 6;
pub const ACTIONS_LEN: usize = 8;
pub const DECL_ARGS_LEN: usize = 4;
// TODO Give this a more generic name
pub const PROPERTY_DECLS_LEN: usize = 4;
pub const FUNCTIONS_DECLS_LEN: usize = 6;
pub const ENUM_CONSTANTS_LEN: usize = 8;
pub const CALLED_ARGS_LEN: usize = DECL_ARGS_LEN;
pub type StructId = TextId;
pub type SVMultiMap<K, V, const N: usize> = HashMap<K, SmallVec<[V; N]>>;

pub struct Compiler {
    dummy_values: DummyReportValues,
    database: CompilerDatabase,
    source_cache: SourceCache,
    main_file: PathBuf,
    src_dir: PathBuf,
}

struct CompilerParseResult<'a, T> {
    pub cst: Vec<(&'a PathBuf, &'a String, Option<T>)>,
    pub lexer_errors: Vec<Rich<'a, char>>,
    pub parser_errors: Vec<Rich<'a, Token, Span>>,
}

impl<'a, T> CompilerParseResult<'a, T> {
    pub fn new() -> CompilerParseResult<'a, T> {
        CompilerParseResult {
            parser_errors: Vec::new(),
            lexer_errors: Vec::new(),
            cst: Vec::new(),
        }
    }
}

pub fn create_path_from_path_buf(
    interner: &impl StringInterner,
    prefix: &PathBuf,
    path: &PathBuf,
) -> cst::PathName {
    let segments: Vec<_> = path
        .strip_prefix(prefix)
        .expect("Only valid prefixes should be used")
        .components()
        .map(|component| {
            let string = component.as_os_str().to_string_lossy().to_string();
            let string = string
                .split(".")
                .next()
                .map(|it| it.to_owned())
                .unwrap_or(String::default());
            interner.intern_string(string)
        })
        .collect();
    cst::PathName { segments }
}

const SRC_DIR_NAME: &str = "src";
const NATIVE_DIR_NAME: &str = "native";
const MAIN_FILE_NAME: &str = "main.co";

impl Compiler {
    pub fn new(project_dir: PathBuf) -> Compiler {
        let src_dir = project_dir.join(SRC_DIR_NAME);

        let database = CompilerDatabase::default();
        let dummy_values = DummyReportValues::new(&database);
        let mut compiler = Compiler {
            database,
            source_cache: SourceCache::new(src_dir.clone()),
            main_file: PathBuf::new(),
            src_dir,
            dummy_values,
        };

        compiler.set_main_file_path(MAIN_FILE_NAME);

        let impl_path = project_dir.join(NATIVE_DIR_NAME);
        let elements = loader::read_impls(&impl_path);

        compiler.database.set_input_wscript_impls(elements);

        compiler
    }

    pub fn set_main_file_path(&mut self, name: &str) {
        self.main_file = self.src_dir.join(name);
        self.database.set_main_file_name_with_durability(
            create_path_from_path_buf(&self.database, &self.src_dir, &self.main_file),
            Durability::HIGH,
        );
    }

    pub fn add_external_file(&mut self, path_buf: PathBuf) {
        self.source_cache.directories.push(path_buf);
    }

    pub fn compile(&mut self) -> (Vec<u8>, Vec<u8>) {
        let result = Self::parse(&mut self.source_cache, &self.database);
        let secondary_files: LinkedHashMap<_, _> = result
            .cst
            .into_iter()
            .map(|(path, content, cst)| {
                let path = create_path_from_path_buf(&self.database, &self.src_dir, &path);
                (path, cst.unwrap_or(cst::Ast::new()))
            })
            .collect();

        println!(
            "Following paths were imported: {}",
            secondary_files
                .keys()
                .map(|it| it.name(&self.database))
                .intersperse("\n".to_string())
                .collect::<String>()
        );

        self.database.set_secondary_files(secondary_files);

        println!("Src dir: {}", self.src_dir.display());
        println!("Main file: {}", self.main_file.display());

        let workshop_output = self.database.query_workshop_output();
        match workshop_output {
            Trisult::Ok(value) => (format!("{value}").into_bytes(), Vec::new()),
            Trisult::Par(_, errors) | Trisult::Err(errors) => {
                let unique_errors = errors.into_iter().collect();
                let source_cache = LookupSourceCache {
                    source_cache: &mut self.source_cache,
                    interner: &self.database,
                    src_dir: &self.src_dir,
                };

                let output_buffer = new_print_errors(
                    unique_errors,
                    source_cache,
                    &self.database,
                    &self.dummy_values,
                );
                (Vec::new(), output_buffer)
            }
        }
    }

    fn parse<'a>(
        source_cache: &'a mut SourceCache,
        database: &'a CompilerDatabase,
    ) -> CompilerParseResult<'a, cst::Ast> {
        let files = source_cache.update_files().unwrap();

        let mut parse_result = CompilerParseResult::new();
        for (path, file) in files {
            let span_source_id = database.intern_span_source(path.clone());

            use language::lexer::lexer as colomar_lexer;
            let (output, mut lexer_errors) = colomar_lexer(span_source_id, database)
                .parse(&file.content)
                .into_output_errors();
            parse_result.lexer_errors.append(&mut lexer_errors);

            if let Some(tokens) = output {
                use language::parser::parser as colomar_parser;

                let eoi = Span::new(
                    span_source_id,
                    CopyRange::from(tokens.len()..tokens.len() + 1),
                );
                let stream = ChumskyStream::from_iter(tokens.into_iter()).spanned(eoi);
                let (output, mut parser_errors) =
                    colomar_parser().parse(stream).into_output_errors();
                parse_result.parser_errors.append(&mut parser_errors);

                parse_result.cst.push((&path, &file.content, output));
            } else {
                parse_result.cst.push((&path, &file.content, None));
            }
        }

        parse_result
    }
}

pub struct SVMultiMapWrapper<K, V, const N: usize>(SVMultiMap<K, V, N>);

impl<K, V, const N: usize> FromIterator<(K, V)> for SVMultiMapWrapper<K, V, N>
where
    K: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let mut map = SVMultiMap::new();

        for (key, value) in iter {
            map.entry(key)
                .or_insert(SmallVec::<[V; N]>::new())
                .push(value);
        }

        SVMultiMapWrapper(map)
    }
}

impl<K, V, const N: usize> FromIterator<(K, SmallVec<[V; N]>)> for SVMultiMapWrapper<K, V, N>
where
    K: Eq + Hash,
{
    fn from_iter<T: IntoIterator<Item = (K, SmallVec<[V; N]>)>>(iter: T) -> Self {
        let mut map = SVMultiMap::new();

        for (key, mut value) in iter {
            map.entry(key)
                .or_insert(SmallVec::<[V; N]>::new())
                .append(&mut value)
        }

        SVMultiMapWrapper(map)
    }
}

impl<K, V, const N: usize> Into<SVMultiMap<K, V, N>> for SVMultiMapWrapper<K, V, N> {
    fn into(self) -> SVMultiMap<K, V, N> {
        self.0
    }
}

pub fn flatten<LR, L, R, const LN: usize, const RN: usize>(
    iter: impl IntoIterator<Item = LR>,
    splitter: impl Fn(LR) -> (SmallVec<[L; LN]>, SmallVec<[R; RN]>),
) -> (SmallVec<[L; LN]>, SmallVec<[R; RN]>) {
    let mut left_vec: SmallVec<[L; LN]> = SmallVec::new();
    let mut right_vec: SmallVec<[R; RN]> = SmallVec::new();

    for lr in iter {
        let (mut left, mut right) = splitter(lr);
        left_vec.append(&mut left);
        right_vec.append(&mut right);
    }

    (left_vec, right_vec)
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Ident {
    pub value: TextId,
    pub span: Span,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum UseRestriction {
    /// Can not be assigned directly, but can be accessed.
    GetVar,
    /// Can be assigned, but not be accessed
    SetVar,
    /// Can only be assigned once and be accessed
    Val,
    /// Can be assigned and accessed
    Var,
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub enum Op {
    And,
    Equals,
    NotEquals,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Op::Equals => write!(f, "=="),
            Op::NotEquals => write!(f, "!="),
            Op::And => write!(f, "&&"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssignMod {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for AssignMod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            AssignMod::Add => "Add",
            AssignMod::Sub => "Subtract",
            AssignMod::Mul => "Multiply",
            AssignMod::Div => "Divide",
        };
        write!(f, "{name}")
    }
}

fn compiler_todo<T>(string: impl Into<Cow<'static, str>>, span: Span) -> QueryTrisult<T> {
    QueryTrisult::Err(vec![CompilerError::NotImplemented(string.into(), span)])
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum Expr<T> {
    Chain(T),
    Neg(Box<Expr<T>>),
    And(Box<Expr<T>>, Box<Expr<T>>),
    Or(Box<Expr<T>>, Box<Expr<T>>),
}

impl<T> Expr<T> {
    fn lhs(&self) -> Option<&Expr<T>> {
        match self {
            Expr::Chain(_) => None,
            Expr::Neg(_) => None,
            Expr::And(lhs, _) | Expr::Or(lhs, _) => Some(lhs.as_ref()),
        }
    }

    fn rhs(&self) -> Option<&Expr<T>> {
        match self {
            Expr::Chain(_) => None,
            Expr::Neg(_) => None,
            Expr::And(_, rhs) | Expr::Or(_, rhs) => Some(rhs.as_ref()),
        }
    }
}
