use nessie_lex::range::Located;
use nessie_lex::{Keyword, Symbol};

pub type Token<'a> = nessie_lex::Token<'a, Keyword, Symbol>;

pub type LToken<'source> = Located<Token<'source>>;

#[derive(Keyword, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    #[string("fn")]
    Fn,
    #[string("if")]
    If,
    #[string("else")]
    Else,
    #[string("for")]
    For,
    #[string("while")]
    While,
    #[string("in")]
    In,
    #[string("do")]
    Do,
    #[string("import")]
    Import,
    #[string("type")]
    Type,
    #[string("match")]
    Match,
    #[string("new")]
    New,
    #[string("with")]
    With,
}

#[derive(Symbol, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    #[string("=>")]
    FatArrow,
    #[string("->")]
    Arrow,
    #[string(":=")]
    ColonEqual,
    #[string("::")]
    ColonColon,
    #[string("()")]
    Unit,
    #[string("..")]
    DotDot,
    #[string("=")]
    Equal,
    #[string(":")]
    Colon,
    #[string(".")]
    Dot,
    #[string(",")]
    Comma,
    #[string("+")]
    Plus,
    #[string("(")]
    OpenParen,
    #[string(")")]
    CloseParen,
    #[string("[")]
    OpenSquare,
    #[string("]")]
    CloseSquare,
    #[string("{")]
    OpenCurly,
    #[string("}")]
    CloseCurly,
}
