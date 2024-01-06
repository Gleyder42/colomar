#![feature(map_try_insert)]
#![feature(iter_intersperse)]

use crate::analysis::decl::DeclQuery;
use crate::analysis::interner::Interner;
use crate::database::CompilerDatabase;
use crate::error::PartialCompilerError;
use crate::error_reporter::{new_print_errors, DummyReportValues};
use crate::language::lexer::Token;
use crate::loader::WorkshopScriptLoader;
use crate::printer::PrinterQuery;
use crate::source_cache::{FileFetcher, SourceCache};
use crate::span::{CopyRange, SpanInterner, SpanSourceId, StringInterner};
use chumsky::error::Rich;
use chumsky::input::Input;
use chumsky::input::Stream as ChumskyStream;
use chumsky::span::SimpleSpan;
use chumsky::Parser;
use error::CompilerError;
use hashlink::LinkedHashMap;
use salsa::Durability;
use smallvec::SmallVec;
use span::Span;
use span::StringId;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::io::Cursor;
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

pub type Text = String;
pub type TextId = StringId;
pub type QueryTrisult<T> = Trisult<T, CompilerError>;

pub type PartialQueryTrisult<T> = Trisult<T, PartialCompilerError>;

pub type OwnedRich<T, S = SimpleSpan<usize>> = Rich<'static, T, S>;

pub const CONDITIONS_LEN: usize = 6;
pub const ACTIONS_LEN: usize = 8;
pub const DECL_ARGS_LEN: usize = 4;

pub const DECL_GENERICS_LEN: usize = 1;
// TODO Give this a more generic name
pub const PROPERTY_DECLS_LEN: usize = 4;
pub const FUNCTIONS_DECLS_LEN: usize = 6;
pub const ENUM_CONSTANTS_LEN: usize = 8;
pub const CALLED_ARGS_LEN: usize = DECL_ARGS_LEN;
pub type StructId = TextId;
pub type SVMultiMap<K, V, const N: usize> = HashMap<K, SmallVec<[V; N]>>;

#[macro_export]
macro_rules! parser_alias {
    ($alias:ident, $input:ty, $error:ty) => {
        pub trait $alias<'a, T>: chumsky::Parser<'a, $input, T, $error> {}

        impl<'a, T, P> $alias<'a, T> for P where P: chumsky::Parser<'a, $input, T, $error> {}

        pub type BBoxed<'a, 'b, T> = Boxed<'a, 'b, $input, T, $error>;
    };
}

pub struct Compiler {
    dummy_values: DummyReportValues,
    database: CompilerDatabase,
    source_cache: FileFetcher,
    main_file: PathBuf,
    src_dir: PathBuf,
}

struct CompilerParseResult<T> {
    pub cst: Vec<(PathBuf, Option<T>)>,
    pub lexer_errors: Vec<(SpanSourceId, Vec<Rich<'static, char>>)>,
    pub parser_errors: Vec<(SpanSourceId, Vec<Rich<'static, Token, Span>>)>,
}

impl<T> CompilerParseResult<T> {
    pub fn new() -> CompilerParseResult<T> {
        CompilerParseResult {
            parser_errors: Vec::new(),
            lexer_errors: Vec::new(),
            cst: Vec::new(),
        }
    }
}

pub fn create_path_from_path_buf(
    interner: &dyn StringInterner,
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
            source_cache: FileFetcher::new(src_dir.clone()),
            main_file: PathBuf::new(),
            src_dir,
            dummy_values,
        };

        compiler.set_main_name(MAIN_FILE_NAME);

        let impl_path = project_dir.join(NATIVE_DIR_NAME);
        let elements = loader::read_impls(&impl_path);

        compiler.database.set_input_wscript_impls(elements);

        compiler
    }

    pub fn set_main_name(&mut self, name: &str) {
        self.main_file = self.src_dir.join(name);
        self.database.set_main_file_name_with_durability(
            create_path_from_path_buf(&self.database, &self.src_dir, &self.main_file),
            Durability::HIGH,
        );
    }

    /// Compiles the sources into workshop format.
    ///
    /// Compiler returns two buffers.
    /// - The left (or first) buffer contains data to be printed to [std::io::stdout()]
    /// - The right (or second) buffer contains data to be printed to [std::io::stderr()]
    pub fn compile(&mut self) -> (Vec<u8>, Vec<u8>) {
        let result = parse(&mut self.source_cache, &self.database);

        let secondary_files: LinkedHashMap<_, _> = result
            .cst
            .into_iter()
            .map(|(path, cst)| {
                let path = create_path_from_path_buf(&self.database, &self.src_dir, &path);
                (path, cst.unwrap_or(cst::Cst::new()))
            })
            .collect();

        println!(
            "Following paths were imported: {}",
            secondary_files
                .keys()
                .map(|it| it.name(&self.database))
                .intersperse(",".to_string())
                .collect::<String>()
        );

        println!("Src dir: {}", self.src_dir.display());
        println!("Main file: {}", self.main_file.display());
        self.database.set_secondary_files(secondary_files);

        let mut cache = SourceCache {
            source_cache: &mut self.source_cache,
            interner: &self.database,
            src_dir: &self.src_dir,
        };
        let mut stderr = Cursor::new(Vec::new());

        error_reporter::write_error(
            result.parser_errors,
            &mut cache,
            &self.database,
            |_, error| *error.span(),
            &mut stderr,
        );
        error_reporter::write_error(
            result.lexer_errors,
            &mut cache,
            &self.database,
            |id, error| (id, error.span().into_range()),
            &mut stderr,
        );

        let workshop_output = self.database.query_workshop_output();
        match workshop_output {
            Trisult::Ok(value) => (format!("{value}").into_bytes(), Vec::new()),
            Trisult::Par(_, errors) | Trisult::Err(errors) => {
                let unique_errors = errors.into_iter().collect();

                new_print_errors(
                    unique_errors,
                    &self.database,
                    cache,
                    &mut self.dummy_values,
                    &mut stderr,
                );

                (Vec::new(), stderr.into_inner())
            }
        }
    }
}

fn parse<'a>(
    source_cache: &'a mut FileFetcher,
    interner: &'a CompilerDatabase,
) -> CompilerParseResult<cst::Cst> {
    let files = source_cache.update_files().unwrap();

    let mut parse_result = CompilerParseResult::new();
    for (path, file) in files.iter() {
        let span_source_id = interner.intern_span_source(path.clone());

        use language::lexer::lexer as colomar_lexer;
        let (output, lexer_errors) = colomar_lexer(span_source_id, interner)
            .parse(&file.content)
            .into_output_errors();

        parse_result
            .lexer_errors
            .push((span_source_id, into_owned(lexer_errors)));

        if let Some(tokens) = output {
            use language::parser::parser as colomar_parser;

            let eoi = Span::new(
                span_source_id,
                CopyRange::from(tokens.len()..tokens.len() + 1),
            );
            let stream = ChumskyStream::from_iter(tokens.into_iter()).spanned(eoi);
            let (output, parser_errors) = colomar_parser().parse(stream).into_output_errors();

            parse_result
                .parser_errors
                .push((span_source_id, into_owned(parser_errors)));

            parse_result.cst.push((path.clone(), output));
        } else {
            parse_result.cst.push((path.clone(), None));
        }
    }

    parse_result
}

pub trait InternedName {
    fn name(&self, interner: &dyn Interner) -> String;
}

pub fn into_owned<T: Clone, S>(input: Vec<Rich<T, S>>) -> Vec<Rich<'static, T, S>> {
    input
        .into_iter()
        .map(|rich| rich.into_owned::<'static>())
        .collect()
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

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
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
