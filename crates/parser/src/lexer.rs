use crate::{SyntaxKind, T};
use SyntaxKind::*;

trait StrExt: Sized {
    fn take(&mut self, on: impl FnMut(char) -> bool) -> Self;
    fn split_off(&mut self, at: usize) -> Self;
    fn take_skip(&mut self, n: usize, mut on: impl FnMut(char) -> bool) -> Self {
        let mut skip = (0..n).map(|_| true);
        self.take(|ch| skip.next().unwrap_or_else(|| on(ch)))
    }
}

impl StrExt for &str {
    fn split_off(&mut self, at: usize) -> Self {
        let (a, b) = self.split_at(at);
        *self = b;
        a
    }

    fn take(&mut self, mut on: impl FnMut(char) -> bool) -> Self {
        let i = self.find(|ch| !on(ch)).unwrap_or(self.len());
        self.split_off(i)
    }
}

macro_rules! any {
    ($($t:tt)*) => {
        ::char_classes::any!($($t)*)
    };
}

fn string_escape(src: &str) -> usize {
    let mut chars = src.char_indices();
    assert_eq!(chars.next(), Some((0, '"')));
    while let Some((i, ch)) = chars.next() {
        if ch == '\\' {
            chars.next();
            continue;
        }
        if ch == '"' {
            return i + 1;
        }
    }
    src.len()
}

fn regex_escape(src: &str) -> usize {
    let mut chars = src.char_indices().peekable();
    assert_eq!(chars.next(), Some((0, '/')));
    while let Some((i, ch)) = chars.next() {
        if ch == '\\' && chars.next_if(|it| it.1 == '/').is_some() {
            continue;
        }
        if ch == '/' {
            return i + 1;
        }
    }
    src.len()
}

fn lex<'input>(source: &mut &'input str) -> Option<(SyntaxKind, &'input str)> {
    let Some(ch) = source.chars().next() else { return None };
    let mut once = true;
    let mut once = |a, end| {
        once && (a || end && std::mem::take(&mut once))
    };

    let (kind, content) = if any!(" \t\r\n", ch) {
        (WHITESPACE, source.take(any!(" \t\r\n")))
    } else if source.starts_with("//") {
        (COMMENT, source.take(any!(^"\n")))
    } else if any!("a-zA-Z_", ch) {
        (IDENT, source.take(any!("a-zA-Z0-9_")))
    } else if ch == '@' {
        (STYLE, source.take_skip(1, any!("a-zA-Z0-9_")))
    } else if any!("0-9", ch) {
        (NUMBER, source.take(any!("0-9")))
    } else if ch == '#' && source.chars().skip(1).find(any!(!"0-9a-zA-Z_")) == Some('#') {
        (BUILTIN, source.take_skip(1, |ch| once(any!("0-9a-zA-Z_", ch), ch == '#')))
    } else if ch == '#' {
        (COLOR, source.take_skip(1, any!("0-9a-fA-F")))
    } else if ch == '<' {
        (MARK, source.take(|ch| once(any!("0-9a-zA-Z_<", ch), ch == '>')))
    } else if ch == '"' {
        (STRING, source.split_off(string_escape(source)))
    } else if ch == '/' {
        (REGEX, source.split_off(regex_escape(source)))
    } else {
        let syntax_kind = match ch {
            ':' => T![:],
            ',' => T![,],
            '{' => T!['{'],
            '}' => T!['}'],
            '[' => T!['['],
            ']' => T![']'],
            '(' => T!['('],
            ')' => T![')'],
            _ => ERROR,
        };
        (syntax_kind, source.split_off(ch.len_utf8()))
    };
    assert!(!content.is_empty(), "{kind:?}");
    Some((kind, content))
}

pub struct Lexer<'input> {
    source: &'input str,
    current: Option<(SyntaxKind, &'input str)>,
}

impl<'input> Lexer<'input> {
    pub fn new(source: &'input str) -> Self {
        Self { source, current: None }
    }

    pub fn current(&mut self) -> Option<(SyntaxKind, &'input str)> {
        if let Some(current) = self.current {
            return Some(current);
        }
        self.current = lex(&mut self.source);
        self.current
    }

    pub fn kind(&mut self) -> Option<SyntaxKind> {
        self.current().map(|it| it.0)
    }

    pub fn token(&mut self) -> Option<&'input str> {
        self.current().map(|it| it.1)
    }

    pub fn bump_any(&mut self) -> bool {
        self.current();
        self.current.take().is_some()
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;
    use expect_test::{Expect, expect};

    use super::*;

    #[track_caller]
    fn check(src: &str, expect: Expect) {
        let mut lex = Lexer::new(src);
        let mut tokens = String::new();
        while let Some((kind, content)) = lex.current() {
            let kind = format!("{kind:?}");
            writeln!(tokens, "{kind:10} {content:?}").unwrap();
            assert!(lex.bump_any());
        }
        expect.assert_eq(&tokens);
    }

    #[test]
    fn basic_lex() {
        check("", expect![]);
        check(" ", expect![[r#"
            WHITESPACE " "
        "#]]);
        check("2a2", expect![[r#"
            NUMBER     "2"
            IDENT      "a2"
        "#]]);
        check("2: // foo", expect![[r#"
            NUMBER     "2"
            COLON      ":"
            WHITESPACE " "
            COMMENT    "// foo"
        "#]]);
        check("2: // foo\n    3", expect![[r#"
            NUMBER     "2"
            COLON      ":"
            WHITESPACE " "
            COMMENT    "// foo"
            WHITESPACE "\n    "
            NUMBER     "3"
        "#]]);
    }

    #[test]
    fn escape_regex() {
        check(r#"/a/"#, expect![[r#"
            REGEX      "/a/"
        "#]]);
        check(r#"/a\nb/"#, expect![[r#"
            REGEX      "/a\\nb/"
        "#]]);
        check(r#"/a\/b/"#, expect![[r#"
            REGEX      "/a\\/b/"
        "#]]);
        check(r#"/a\\/b/"#, expect![[r#"
            REGEX      "/a\\\\/b/"
        "#]]);
        check(r#"/a\\/b//c/"#, expect![[r#"
            REGEX      "/a\\\\/b/"
            REGEX      "/c/"
        "#]]);
    }

    #[test]
    fn escape_string() {
        check(r#""""#, expect![[r#"
            STRING     "\"\""
        "#]]);
        check(r#""a""#, expect![[r#"
            STRING     "\"a\""
        "#]]);
        check(r#""a\nb""#, expect![[r#"
            STRING     "\"a\\nb\""
        "#]]);
        check(r#""a\"b""#, expect![[r#"
            STRING     "\"a\\\"b\""
        "#]]);
        check(r#""a\\b"2"#, expect![[r#"
            STRING     "\"a\\\\b\""
            NUMBER     "2"
        "#]]);
    }

    #[test]
    fn full() {
        check(r#"
            ident"string"/regex/234#ff1b1B<mark>#builtin#@style
            ,:[]{}()// comment
        "#, expect![[r##"
            WHITESPACE "\n            "
            IDENT      "ident"
            STRING     "\"string\""
            REGEX      "/regex/"
            NUMBER     "234"
            COLOR      "#ff1b1B"
            MARK       "<mark>"
            BUILTIN    "#builtin#"
            STYLE      "@style"
            WHITESPACE "\n            "
            COMMA      ","
            COLON      ":"
            L_BRACK    "["
            R_BRACK    "]"
            L_CURLY    "{"
            R_CURLY    "}"
            L_PAREN    "("
            R_PAREN    ")"
            COMMENT    "// comment"
            WHITESPACE "\n        "
        "##]]);
    }

    #[test]
    fn splitted() {
        check(r#"
            @a@b
            /a//b/
            "a""b"
            #b##b#
            <mark><mark>
            (()){{}}[[]]
        "#, expect![[r##"
            WHITESPACE "\n            "
            STYLE      "@a"
            STYLE      "@b"
            WHITESPACE "\n            "
            REGEX      "/a/"
            REGEX      "/b/"
            WHITESPACE "\n            "
            STRING     "\"a\""
            STRING     "\"b\""
            WHITESPACE "\n            "
            BUILTIN    "#b#"
            BUILTIN    "#b#"
            WHITESPACE "\n            "
            MARK       "<mark>"
            MARK       "<mark>"
            WHITESPACE "\n            "
            L_PAREN    "("
            L_PAREN    "("
            R_PAREN    ")"
            R_PAREN    ")"
            L_CURLY    "{"
            L_CURLY    "{"
            R_CURLY    "}"
            R_CURLY    "}"
            L_BRACK    "["
            L_BRACK    "["
            R_BRACK    "]"
            R_BRACK    "]"
            WHITESPACE "\n        "
        "##]]);
    }
}
