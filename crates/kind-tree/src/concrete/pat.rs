//! All of the definitons of patterns are described
//! here. It's really useful to split between patterns
//! and expressions in order to restrict even more because
//! not all of the expressions can be turned into patterns.

use std::fmt::{Display, Error, Formatter};

use kind_span::Range;

use crate::symbol::{Ident, QualifiedIdent};

// Really useful thin layer on ident.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PatIdent(pub Ident);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PatKind {
    /// Name of a variable
    Var(PatIdent),
    /// Application of a constructor
    App(QualifiedIdent, Vec<Box<Pat>>),
    /// 60 bit unsigned integer
    U60(u64),
    /// 120 bit unsigned integer
    U120(u128),
    /// 60 bit floating point number
    F60(u64),
    /// Pair
    Pair(Box<Pat>, Box<Pat>),
    /// List
    List(Vec<Pat>),
    /// Str
    Str(String),
    ///
    Char(char),
    /// Wildcard
    Hole,
}

/// Describes a single `pattern`
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Pat {
    pub data: PatKind,
    pub range: Range,
}

impl Display for Pat {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use PatKind::*;
        match &self.data {
            Var(name) => write!(f, "{}", name.0),
            App(head, spine) => write!(
                f,
                "({}{})",
                head,
                spine.iter().map(|x| format!(" {}", x)).collect::<String>()
            ),
            List(vec) => write!(
                f,
                "[{}]",
                vec.iter()
                    .map(|x| format!("{}", x))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Str(str) => write!(f, "\"{}\"", str),
            U60(num) => write!(f, "{}", num),
            U120(num) => write!(f, "{}u120", num),
            F60(_num) => todo!(),
            Char(chr) => write!(f, "\'{}\'", chr),
            Pair(fst, snd) => write!(f, "({}, {})", fst, snd),
            Hole => write!(f, "_"),
        }
    }
}
