use parser::{T, support};
use parser::{AstNode, NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::AstToken;

macro_rules! impl_ast_token_for_enum {
    ($name:ident $(. $variant:ident)+ for $parent:ident.$method:ident) => {
        impl $parent {
            pub fn $method(&self) -> Option<$name> {
                self.syntax().children_with_tokens()
                    .filter_map(NodeOrToken::into_token)
                    .find_map(|it| match it.kind() {
                        $(SyntaxKind::$variant => Some($name::$variant(it)),)+
                        _ => None,
                    })
            }
        }

        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($variant(SyntaxToken),)+
        }

        paste::paste! {
            impl $name {
                $(pub fn [<into_ $variant:lower>](self) -> Option<SyntaxToken> {
                    #[allow(unused)]
                    match self {
                        Self::$variant(it) => Some(it),
                        _ => None,
                    }
                })*
            }
        }

        impl crate::AstToken for $name {
            fn syntax(&self) -> SyntaxToken {
                match self {
                    $(Self::$variant(it) => it.clone(),)+
                }
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.syntax().fmt(f)
            }
        }
    };
}

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

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.syntax().fmt(f)
            }
        }

        impl $name {
            $(define_nodes!(@impl $name $method $child);)*
            $(define_nodes!(@impl*$name $rmethod $rchild);)*
        }
    };
    (@enum $name:ident . $kinds:ident) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct $name(SyntaxNode);
    };
    (@enum $name:ident $(. $kinds:ident)+) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum $name {
            $($kinds($kinds),)*
        }

        paste::paste! {
            impl $name {
                $(pub fn [<into_ $kinds:lower>](self) -> Option<$kinds> {
                    #[allow(unused)]
                    match self {
                        Self::$kinds(it) => Some(it),
                        _ => None,
                    }
                })*
            }
        }
    };
    (@impl $name:ident $method:ident $child:ident) => {
        pub fn $method(&self) -> Option<$child> {
            support::child(&self.0)
        }
    };
    (@impl*$name:ident $method:ident $child:ident) => {
        pub fn $method(&self) -> parser::ast::AstChildren<$child> {
            support::children(&self.0)
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

impl_ast_token_for_enum!(Key.IDENT.NUMBER for Pair.key);
impl_ast_token_for_enum!(CallName.IDENT for Call.name);
impl_ast_token_for_enum!(Lit.BUILTIN.STRING.NUMBER.REGEX.MARK.COLOR.STYLE.IDENT for Literal.lit);

impl Item {
    fn value_pair(&self) -> (Option<Value>, Option<SyntaxToken>, Option<Value>) {
        let node = self.syntax();
        let sep
            = support::token(node, T![:]).or_else(|| {
                support::token(node, T![>])
            });
        let mut children = support::children(node).peekable();
        let value = children.next_if(|it: &Value| {
            sep.as_ref().is_none_or(|sep| {
                it.syntax().text_range().start() < sep.text_range().start()
            })
        });
        let assoc = children.next();
        (value, sep, assoc)
    }

    pub fn value(&self) -> Option<Value> {
        self.value_pair().0
    }

    pub fn assoc(&self) -> Option<Value> {
        self.value_pair().2
    }

    pub fn sep(&self) -> Option<SyntaxToken> {
        self.value_pair().1
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Or<A, B> {
    A(A),
    B(B),
}
impl<A, B> AstNode for Or<A, B>
where
    A: AstNode<Language = parser::Language>,
    B: AstNode<Language = parser::Language>,
{
    type Language = parser::Language;

    fn cast(node: SyntaxNode) -> Option<Self> {
        if A::can_cast(node.kind()) {
            Some(Self::A(A::cast(node)?))
        } else if B::can_cast(node.kind()) {
            Some(Self::B(B::cast(node)?))
        } else {
            None
        }
    }

    fn can_cast(kind: SyntaxKind) -> bool {
        A::can_cast(kind) || B::can_cast(kind)
    }

    fn syntax(&self) -> &SyntaxNode {
        match self {
            Or::A(a) => a.syntax(),
            Or::B(b) => b.syntax(),
        }
    }
}
