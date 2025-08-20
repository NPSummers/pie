use memchr::memchr2;
use std::borrow::Cow;

fn unescape(c: char) -> Option<char> {
    Some(match c {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '0' => '\0',
        '\'' => '\'',
        '\"' => '\"',
        _ => return None,
    })
}

fn unescape_string<'s>(lex: &mut Lexer<'s, Token<'s>>) -> Option<Cow<'s, str>> {
    let remainder = lex.remainder();
    let first_important = memchr2(b'\\', b'"', remainder.as_bytes())?;
    // Early return on string without escapes
    if remainder.as_bytes()[first_important] == b'"' {
        lex.bump(first_important + 1);
        return Some(Cow::Borrowed(&remainder[..first_important]));
    }
    let mut out = String::from(&remainder[..first_important]);
    let mut i = first_important + 1;
    let escaped = remainder[i..].chars().next()?;
    out.push(unescape(escaped)?);
    i += escaped.len_utf8();
    loop {
        let haystack = &remainder[i..];
        let important = memchr2(b'\\', b'"', haystack.as_bytes())?;
        i += important;
        // Capture string component before escape/quote
        out.push_str(&haystack[..important]);
        if haystack.as_bytes()[important] == b'"' {
            lex.bump(i + 1);
            return Some(Cow::Owned(out));
        }
        let escaped = haystack[important + 1..].chars().next()?;
        i += 1;
        out.push(unescape(escaped)?);
        i += escaped.len_utf8();
    }
}

const fn split(sep: &'static str) -> impl for<'s> Fn(&mut Lexer<'s, Token<'s>>) -> Vec<&'s str> {
    move |lex| lex.slice().split(sep).collect()
}

fn remove_underscores<'s>(s: &'s str) -> Cow<'s, str> {
    let Some(first_underscore) = memchr::memchr(b'_', s.as_bytes()) else {
        return s.into();
    };
    let mut out = s[..first_underscore].to_string();
    let mut rest = &s[first_underscore + 1..];
    while let Some(first_underscore) = memchr::memchr(b'_', rest.as_bytes()) {
        out.push_str(&rest[..first_underscore]);
        rest = &rest[first_underscore + 1..];
    }
    out.push_str(rest);
    out.into()
}

use logos::{Lexer, Logos};

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\r]+")]
#[logos(skip r"#.*")]
pub enum Token<'s> {
    // keywords
    #[token("let")]
    /// let
    Let,
    #[token("if")]
    /// if
    If,
    #[token("elif")]
    /// elif
    Elif,
    #[token("else")]
    /// else
    Else,
    #[token("use")]
    /// use
    Use,
    #[token("for")]
    /// for
    For,
    #[token("in")]
    /// in
    In,
    #[token("while")]
    /// while
    While,
    #[token("module")]
    /// module
    Module,
    #[token("def")]
    /// def
    Def,
    #[token("return")]
    /// return
    Return,
    #[token("break")]
    /// break
    Break,
    #[token("continue")]
    /// continue
    Continue,

    // literals
    #[token("true", |_| true)]
    #[token("false", |_| false)]
    BoolLit(bool),
    #[regex("\"", unescape_string)]
    StringLit(Cow<'s, str>),
    #[regex(r"0x[0-9a-fA-F][0-9a-fA-F_]*", |lex| i64::from_str_radix(&remove_underscores(&lex.slice()[2..]), 16).ok())]
    #[regex(r"0o[0-7_]+", |lex| i64::from_str_radix(&remove_underscores(&lex.slice()[2..]), 8).ok())]
    #[regex(r"0b[0-1_]+", |lex| i64::from_str_radix(&remove_underscores(&lex.slice()[2..]), 2).ok())]
    #[regex(r"[0-9][0-9_]*", |lex| remove_underscores(lex.slice()).parse().ok())]
    IntLit(i64),
    #[regex(r"\.\d+", |lex| lex.slice().parse().ok())]
    #[regex(r"\d+\.\d+", |lex| lex.slice().parse().ok())]
    FloatLit(f64),
    #[regex(r"'\\?[^']'", (|lex: &mut Lexer<'_>| -> Option<char> {
        let text = lex.slice();
        let text = &text[1..text.len() - 1];
        let mut chars = text.chars();
        let c = chars.next().unwrap();
        if c != '\\' {
            return Some(c);
        };
        let c = chars.next().unwrap();
        unescape(c)
    }))]
    Char(char),

    // identifiers
    #[regex(
        r"[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)+",
        priority = 2,
        callback = split(".")
    )]
    MemberAccess(Vec<&'s str>),
    #[regex(
        r"[A-Za-z_][A-Za-z0-9_]*(::[A-Za-z_][A-Za-z0-9_]*)+",
        priority = 1,
        callback = split("::")
    )]
    ModuleAccess(Vec<&'s str>),
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", priority = 0)]
    Ident(&'s str),

    // multi-char punctuation
    #[token("==")]
    /// ==
    EqEq,
    #[token("!=")]
    /// ==
    Ne,
    #[token("<=")]
    /// ==
    LtEq,
    #[token(">=")]
    /// ==
    GtEq,
    #[token("+=")]
    /// +=
    PlusEq,
    #[token("-=")]
    /// -=
    MinusEq,
    #[token("*=")]
    /// *=
    StarEq,
    #[token("/=")]
    /// /=
    SlashEq,
    #[token("&&")]
    /// &&
    And,
    #[token("||")]
    /// ||
    Or,

    // single-char tokens
    #[token("<")]
    /// <
    Lt,
    #[token(">")]
    /// >
    Gt,
    #[token("%")]
    /// %
    Percent,
    #[token("+")]
    /// +
    Plus,
    #[token("-")]
    /// -
    Minus,
    #[token("*")]
    /// *
    Star,
    #[token("/")]
    /// /
    Slash,
    #[token(":")]
    /// :
    Colon,
    #[token("(")]
    /// (
    LParen,
    #[token(")")]
    /// )
    RParen,
    #[token("{")]
    /// {
    LBrace,
    #[token("}")]
    /// }
    RBrace,
    #[token("[")]
    /// [
    LBracket,
    #[token("]")]
    /// ]
    RBracket,
    #[token(",")]
    /// ,
    Comma,
    #[token(";")]
    /// ;
    Semicolon,
    #[token(".")]
    /// .
    Dot,
    #[token("=")]
    /// =
    Eq,
    #[token("!")]
    /// !
    Not,
}
