pub use parser::{AstNode, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken};

macro_rules! define_nodes {
    {$(
        $name:ident $(. $kinds:ident)+ [$($t:tt)*];
    )*} => {
        $(
            define_nodes!(@struct $name $(. $kinds)+ [$($t)*]);
        )*
    };
    (@struct $name:ident $(. $kinds:ident)+ [
        $(  $method:ident .  $child:ident),*
        $(*$rmethod:ident . $rchild:ident),*
        $(,)?
    ]) => {
        define_nodes!(@enum $name $(. $kinds)+);
        define_nodes!(@AstNode $name $(. $kinds)+);

        impl $name {
            $(define_nodes!(@impl $name $method $child);)*
            $(define_nodes!(@impl*$name $rmethod $rchild);)*
        }
    };
    (@enum $name:ident . $kinds:ident) => {
        pub struct $name(SyntaxNode);
    };
    (@enum $name:ident $(. $kinds:ident)+) => {
        pub enum $name {
            $($kinds($kinds),)*
        }
    };
    (@impl $name:ident $method:ident $child:ident) => {
        pub fn $method(&self) -> Option<$child> {
            parser::support::child(&self.0)
        }
    };
    (@impl*$name:ident $method:ident $child:ident) => {
        pub fn $method(&self) -> parser::ast::AstChildren<$child> {
            parser::support::children(&self.0)
        }
    };
    (@AstNode $name:ident . $kinds:ident) => {
        impl AstNode for $name {
            type Language = parser::Language;

            fn cast(node: SyntaxNode) -> Option<Self> {
                Self::can_cast(node.kind()).then(|| $name(node))
            }

            fn can_cast(kind: SyntaxKind) -> bool {
                matches!(kind, SyntaxKind::$kinds)
            }

            fn syntax(&self) -> &SyntaxNode {
                &self.0
            }
        }
    };
    (@AstNode $name:ident $(. $kinds:ident)+) => {
        impl AstNode for $name {
            type Language = parser::Language;

            fn cast(node: SyntaxNode) -> Option<Self> {
                $(if $kinds::can_cast(node.kind()) {
                    Some(Self::$kinds($kinds(node)))
                } else)+ {
                    None
                }
            }

            fn can_cast(kind: SyntaxKind) -> bool {
                $($kinds::can_cast(kind))||+
            }

            fn syntax(&self) -> &SyntaxNode {
                match self {
                    $(Self::$kinds(it) => it.syntax()),+
                }
            }
        }
    };
    ($($t:tt)*) => {
        compile_error!(stringify!($($t)*));
    };
}

define_nodes! {
    SourceFile.SOURCE_FILE [table.Table];
    Table.TABLE [*pairs.Pair];
    Pair.PAIR [value.Value];
    Literal.LITERAL [];
    Join.JOIN [];
    Array.ARRAY [*items.Item];
    Item.ITEM [];
    Call.CALL [];
    Value.Join.Table.Array.Literal.Call [];
}

pub enum Key {
    Ident(SyntaxToken),
    Number(SyntaxToken),
}

impl Pair {
    pub fn key(&self) -> Option<Key> {
        self.syntax().children_with_tokens()
            .filter_map(NodeOrToken::into_token)
            .find_map(|it| match it.kind() {
                SyntaxKind::IDENT => Some(Key::Ident(it)),
                SyntaxKind::NUMBER => Some(Key::Number(it)),
                _ => None,
            })
    }
}
