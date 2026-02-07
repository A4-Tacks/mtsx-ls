mod lexer;

pub type SyntaxNode = rowan::SyntaxNode<Language>;
pub type SyntaxToken = rowan::SyntaxToken<Language>;

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Language {}
impl rowan::Language for Language {
    type Kind = SyntaxKind;

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= Self::Kind::ERROR as u16, "{raw:?}");
        unsafe { std::mem::transmute::<u16, Self::Kind>(raw.0) }
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    TABLE = 0,
    ARRAY,
    PAIR,

    IDENT,   // a
    STRING,  // "a"
    REGEX,   // /a/
    NUMBER,  // 123
    COLOR,   // #ff1b1b
    BUILTIN, // #xxx#
    MARK,    // <abc>
    STYLE,   // @B

    COMMA,
    COLON,
    L_CURLY,
    R_CURLY,
    L_BRACK,
    R_BRACK,
    L_PAREN,
    R_PAREN,

    WHITESPACE,
    COMMENT,

    ERROR,
}
impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(value: SyntaxKind) -> Self {
        rowan::SyntaxKind(value as u16)
    }
}

/// T![]
#[macro_export]
macro_rules! T {
    (,)   => { $crate::SyntaxKind::COMMA };
    (:)   => { $crate::SyntaxKind::COLON };
    ('{') => { $crate::SyntaxKind::L_CURLY };
    ('}') => { $crate::SyntaxKind::R_CURLY };
    ('[') => { $crate::SyntaxKind::L_BRACK };
    (']') => { $crate::SyntaxKind::R_BRACK };
    ('(') => { $crate::SyntaxKind::L_PAREN };
    (')') => { $crate::SyntaxKind::R_PAREN };
}
