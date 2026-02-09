use either::Either::{Left, Right};
use rowan::{GreenNode, GreenNodeBuilder, GreenNodeData, GreenToken, NodeOrToken, TextRange, TextSize};

use crate::{SyntaxKind, SyntaxNode, T, lexer::Lexer};
use SyntaxKind::*;

// SOURCE_FILE  = TABLE
// TABLE        = "{" PAIR* "}"
// PAIR         = key ":" value [","]
// key          = IDENT | NUMBER
// value        = JOIN | TABLE | ARRAY | LITERAL | CALL
// LITERAL      = BUILTIN | STRING | NUMBER | REGEX | MARK | COLOR | IDENT
// JOIN         = value "+" value
// ARRAY        = "{" ITEM "}"
// ITEM         = value [">"] [":" value] [","]
// CALL         = IDENT "(" *(STRING [","]) ")"
pub struct Parser<'input> {
    lexer: Lexer<'input>,
    source: &'input str,
    builder: GreenNodeBuilder<'static>,
    errors: Vec<(TextRange, String)>,
}

impl<'input> Parser<'input> {
    pub fn new(source: &'input str) -> Self {
        Self {
            lexer: Lexer::new(source),
            source,
            errors: vec![],
            builder: GreenNodeBuilder::new(),
        }
    }

    fn bump_trivias(&mut self) {
        while let Some((kind, text)) = self.lexer.current()
            && kind.is_trivia()
        {
            self.builder.token(kind.into(), text);
            self.lexer.bump_any();
        }
    }

    #[track_caller]
    fn bump(&mut self, kind: SyntaxKind) {
        let Some((current, text)) = self.lexer.current() else { panic!("bump eof") };
        assert_eq!(current, kind);
        assert!(!kind.is_trivia(), "{kind:?}");

        self.builder.token(kind.into(), text);
        self.lexer.bump_any();
    }

    fn current(&mut self) -> SyntaxKind {
        self.bump_trivias();
        self.lexer.kind().unwrap_or(SyntaxKind::ERROR)
    }

    fn is_eof(&mut self) -> bool {
        self.current() == ERROR
            && self.lexer.kind().is_none()
    }

    #[must_use]
    fn mark(&mut self) -> rowan::Checkpoint {
        self.bump_trivias();
        self.builder.checkpoint()
    }

    fn node(&mut self, kind: SyntaxKind, mark: rowan::Checkpoint) {
        self.builder.start_node_at(mark, kind.into());
        self.builder.finish_node();
    }

    fn bump_any(&mut self) {
        let current = self.current();
        self.bump(current);
    }

    fn eat(&mut self, kind: SyntaxKind) -> bool {
        if self.current() == kind {
            self.bump(kind);
            true
        } else {
            false
        }
    }

    #[expect(unused)]
    fn bump_expect(&mut self, kind: SyntaxKind) -> bool {
        let current = self.current();
        if current == kind {
            self.bump(kind);
            true
        } else {
            self.bump_error(format_args!(
                    "unexpected {}, expected {}",
                    current.human_readable(),
                    kind.human_readable(),
            ));
            false
        }
    }

    fn bump_or_expect(&mut self, kind: SyntaxKind) -> bool {
        let current = self.current();
        if current == kind {
            self.bump(kind);
            true
        } else {
            self.report_error(format_args!(
                    "unexpected {}, expected {}",
                    current.human_readable(),
                    kind.human_readable(),
            ));
            false
        }
    }

    fn report_error(&mut self, msg: impl ToString) {
        let text = self.lexer.token().unwrap_or("");
        let index = TextSize::try_from(self.lexer.index()).unwrap();
        let range = TextRange::at(index, TextSize::of(text));

        self.errors.push((range, msg.to_string()));
    }

    #[track_caller]
    fn bump_error(&mut self, msg: impl ToString) {
        let mark = self.mark();
        let kind = self.current();
        self.report_error(msg);
        self.bump(kind);
        self.node(ERROR, mark);
    }
}

impl<'input> Parser<'input> {
    fn value(&mut self) {
        let mark = self.mark();

        for nr in 0.. {
            let kind = self.current();
            match kind {
                L_CURLY => self.table(),
                L_BRACK => self.array(),
                IDENT => self.ident_or_call(),
                BUILTIN | STRING | NUMBER | REGEX | MARK | COLOR => {
                    let mark = self.mark();
                    self.bump(kind);
                    self.node(LITERAL, mark);
                },
                _ if kind.is_close_delim() => {
                    self.report_error(format_args!("unexpected {}, expected value", kind.human_readable()));
                }
                _ => {
                    self.bump_error(format_args!("unexpected {}, expected value", kind.human_readable()));
                }
            }
            if nr != 0 {
                self.node(JOIN, mark);
            }
            if !self.eat(T![+]) {
                break;
            }
        }
    }

    fn table(&mut self) {
        let mark = self.mark();
        self.bump(L_CURLY);

        while matches!(self.current(), IDENT | NUMBER | T![:]) {
            self.pair();
        }

        self.bump_or_expect(R_CURLY);
        self.node(TABLE, mark);
    }

    fn pair(&mut self) {
        let mark = self.mark();

        if matches!(self.current(), IDENT | NUMBER) {
            self.bump_any();
        } else {
            self.report_error("expected a ident or number");
        }

        if self.bump_or_expect(T![:]) {
            self.value();
        }
        self.eat(T![,]);
        self.node(PAIR, mark);
    }

    fn array(&mut self) {
        let mark = self.mark();
        self.bump(L_BRACK);
        while !self.current().is_close_delim() && !self.is_eof() {
            self.item();
        }
        self.bump_or_expect(R_BRACK);
        self.node(ARRAY, mark);
    }

    fn ident_or_call(&mut self) {
        let mark = self.mark();
        self.bump(IDENT);

        if self.eat(L_PAREN) {
            while self.eat(STRING) {
                self.eat(T![,]);
            }
            self.bump_or_expect(R_PAREN);
            self.node(CALL, mark);
        } else {
            self.node(LITERAL, mark);
        }
    }

    fn item(&mut self) {
        let mark = self.mark();
        self.value();
        self.eat(T![>]);

        if self.eat(T![:]) {
            self.value();
        }

        self.eat(T![,]);
        self.node(ITEM, mark);
    }

    pub fn source_file(&mut self) {
        let mark = self.builder.checkpoint();

        while self.current() != L_CURLY && !self.is_eof() {
            let tok = self.current().human_readable();
            self.bump_error(format_args!("unexpected {tok}, expected `{{`"));
        }

        if self.current() == L_CURLY {
            self.table();
        }

        while !self.is_eof() {
            let tok = self.current().human_readable();
            self.bump_error(format_args!("unexpected {tok}, expected end of file"));
        }

        let _mark_end = self.mark();
        self.node(SOURCE_FILE, mark);
    }

    pub fn finish(self) -> (SyntaxNode, Vec<(TextRange, String)>) {
        let green_node = self.builder.finish();
        let clean_node = clean_childs_trivias(&green_node);
        let node = SyntaxNode::new_root(clean_node);
        assert_eq!(node.text_range().len(), TextSize::of(self.source), "{node:#?}");
        (node, self.errors)
    }
}

fn clean_childs_trivias(node: &GreenNodeData) -> GreenNode {
    let children = node.children()
        .flat_map(|child| match child {
            NodeOrToken::Node(node) => Left(clean_trivias(node)),
            NodeOrToken::Token(tok) => Right([tok.to_owned().into()]),
        }.into_iter())
        .collect::<Vec<_>>();
    GreenNode::new(node.kind(), children)
}

fn clean_trivias(node: &GreenNodeData) -> impl Iterator<Item = NodeOrToken<GreenNode, GreenToken>> {
    let is_solid = |it: NodeOrToken<&GreenNodeData, &rowan::GreenTokenData>| {
        !SyntaxKind::from(it.kind()).is_trivia()
    };
    let Some(first_solid) = node
        .children()
        .position(is_solid)
    else {
        // 对于全由trivia组成的节点, 调整其打开后trivia在前还是后
        let iter = node.children()
            .map(NodeOrToken::to_owned)
            .chain([GreenNode::new(node.kind(), []).into()]);
        return Left(iter);
    };
    let last_solid = node.children().rposition(is_solid).unwrap();

    let first = node.children().take(first_solid);
    let last = node.children().skip(last_solid + 1);
    let children = node.children().take(last_solid + 1).skip(first_solid);
    let trimmed = children.flat_map(|child| {
        match child {
            NodeOrToken::Node(node) => Left(clean_trivias(node)),
            NodeOrToken::Token(tok) => Right([tok.to_owned().into()]),
        }.into_iter()
    });

    Right(first.map(NodeOrToken::to_owned)
        .chain([GreenNode::new(node.kind(), trimmed.collect::<Vec<_>>()).into()])
        .chain(last.map(NodeOrToken::to_owned)))
}

#[cfg(test)]
mod tests;
