pub use text_size::{TextRange, TextSize};
pub use rowan::ast::{self, AstNode, support};
pub use rowan::{NodeOrToken, Direction};

mod lexer;
pub mod parser;

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
        raw.into()
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    SOURCE_FILE = 0,
    TABLE,
    ARRAY,
    PAIR,
    JOIN,
    CALL,
    ITEM,
    LITERAL,

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
    PLUS,
    FAT_ARROW,
    L_CURLY,
    R_CURLY,
    L_BRACK,
    R_BRACK,
    L_PAREN,
    R_PAREN,
    R_ANGLE,

    WHITESPACE,
    COMMENT,
    PREPROC,

    ERROR,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(value: SyntaxKind) -> Self {
        rowan::SyntaxKind(value as u16)
    }
}
impl From<rowan::SyntaxKind> for SyntaxKind {
    fn from(raw: rowan::SyntaxKind) -> Self {
        assert!(raw.0 <= Self::ERROR as u16, "{raw:?}");
        unsafe { std::mem::transmute::<u16, Self>(raw.0) }
    }
}
impl SyntaxKind {
    pub fn is_trivia(self) -> bool {
        matches!(self, Self::COMMENT | Self::WHITESPACE | Self::PREPROC)
    }

    pub fn is_open_delim(self) -> bool {
        matches!(self, Self::L_CURLY | Self::L_BRACK | Self::L_PAREN)
    }

    pub fn is_close_delim(self) -> bool {
        matches!(self, Self::R_CURLY | Self::R_BRACK | Self::R_PAREN)
    }

    pub fn human_readable(self) -> &'static str {
        match self {
            SyntaxKind::SOURCE_FILE => "source file",
            SyntaxKind::TABLE => "table",
            SyntaxKind::ARRAY => "array",
            SyntaxKind::PAIR => "pair",
            SyntaxKind::JOIN => "expr",
            SyntaxKind::CALL => "call",
            SyntaxKind::ITEM => "item",
            SyntaxKind::LITERAL => "literal",
            SyntaxKind::IDENT => "ident",
            SyntaxKind::STRING => "string",
            SyntaxKind::REGEX => "regex",
            SyntaxKind::NUMBER => "number",
            SyntaxKind::COLOR => "color",
            SyntaxKind::BUILTIN => "builtin",
            SyntaxKind::MARK => "mark",
            SyntaxKind::STYLE => "style",
            SyntaxKind::COMMA => "comma",
            SyntaxKind::COLON => "colon",
            SyntaxKind::PLUS => "`+`",
            SyntaxKind::FAT_ARROW => "`=>`",
            SyntaxKind::L_CURLY => "`{`",
            SyntaxKind::R_CURLY => "`}`",
            SyntaxKind::L_BRACK => "`[`",
            SyntaxKind::R_BRACK => "`]`",
            SyntaxKind::L_PAREN => "`(`",
            SyntaxKind::R_PAREN => "`)`",
            SyntaxKind::R_ANGLE => "`>`",
            SyntaxKind::WHITESPACE => "whitespace",
            SyntaxKind::COMMENT => "comment",
            SyntaxKind::PREPROC => "preproc",
            SyntaxKind::ERROR => "error token",
        }
    }
}

/// T![]
#[macro_export]
macro_rules! T {
    (,)   => { $crate::SyntaxKind::COMMA };
    (:)   => { $crate::SyntaxKind::COLON };
    (+)   => { $crate::SyntaxKind::PLUS };
    (>)   => { $crate::SyntaxKind::R_ANGLE };
    (=>)   => { $crate::SyntaxKind::FAT_ARROW };
    ('{') => { $crate::SyntaxKind::L_CURLY };
    ('}') => { $crate::SyntaxKind::R_CURLY };
    ('[') => { $crate::SyntaxKind::L_BRACK };
    (']') => { $crate::SyntaxKind::R_BRACK };
    ('(') => { $crate::SyntaxKind::L_PAREN };
    (')') => { $crate::SyntaxKind::R_PAREN };
}
