use std::fmt::Write;
use text_size::{TextRange, TextSize};
pub use dissimilar;

pub fn clean_inline_lints(src: &str) -> String {
    let has_eol = src.trim_end_matches(' ').ends_with('\n');
    let mut output: String = src.split_inclusive('\n')
        .filter(|line| !line.trim_start().starts_with("// ^"))
        .collect();
    if !has_eol && output.chars().next_back() == Some('\n') {
        output.pop().unwrap();
    }
    output
}

#[track_caller]
pub fn apply_inline_lints(src: &str, mut lints: Vec<(TextRange, String)>) -> String {
    lints.sort_by_key(|(range, lint)| {
        (range.start(), range.len(), lint.len())
    });
    let mut output = String::new();
    let mut real_range = TextRange::empty(TextSize::new(0));

    for line in src.split_inclusive('\n') {
        let line_range = TextRange::at(real_range.end(), TextSize::of(line));
        output += line;

        let has_eol = line.chars().next_back() == Some('\n');
        if !has_eol {
            output += "\n";
        }

        for (range, lint) in &lints {
            if !line_range.contains_range(*range) {
                continue;
            }
            let inline = range - line_range.start();
            let column = inline.start();
            let Some(place) = column.checked_sub(TextSize::of("// ")) else {
                panic!("inline lint on column {column:?} is unsupported (`{lint}`)")
            };
            let indent = " ".repeat(place.into());
            let arrow = "^".repeat(inline.len().into());
            writeln!(output, "{indent}// {arrow} {lint}").unwrap();
        }

        if !has_eol && output.ends_with('\n') {
            output.pop().unwrap();
        }

        real_range = real_range.cover(line_range);
    }

    output
}

pub trait ExpectLike {
    #[track_caller]
    fn assert_eq(&self, actual: &str);

    #[track_caller]
    fn assert_debug_eq(&self, actual: &impl std::fmt::Debug);
}
impl ExpectLike for expect_test::Expect {
    fn assert_eq(&self, actual: &str) {
        self.assert_eq(actual);
    }
    fn assert_debug_eq(&self, actual: &impl std::fmt::Debug) {
        self.assert_debug_eq(actual);
    }
}
impl ExpectLike for expect_test::ExpectFile {
    fn assert_eq(&self, actual: &str) {
        self.assert_eq(actual);
    }
    fn assert_debug_eq(&self, actual: &impl std::fmt::Debug) {
        self.assert_debug_eq(actual);
    }
}

pub fn print_chunks(chunks: Vec<dissimilar::Chunk>) {
    for chunk in chunks {
        let formatted = match chunk {
            dissimilar::Chunk::Equal(text) => text.into(),
            dissimilar::Chunk::Delete(text) => format!("\x1b[41m{}\x1b[0m", text),
            dissimilar::Chunk::Insert(text) => format!("\x1b[42m{}\x1b[0m", text),
        };
        print!("{formatted}")
    }
}

#[macro_export]
macro_rules! assert_text_eq {
    ($a:expr, $b:expr $(, $($t:tt)*)?) => {{
        let (a, b) = (&$a, &$b);
        if a != b {
            $crate::print_chunks($crate::dissimilar::diff(a, b));
            panic!($($($t)*)?);
        }
    }};
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use super::*;

    #[test]
    fn test_clean_inline_lints() {
        let src = r#"
            foo bar baz
             // ^^^ at bar error
                 // ^^^ at baz error
        "#;
        expect![[r#"
            "\n            foo bar baz\n        "
        "#]].assert_debug_eq(&clean_inline_lints(src));
        expect![[r#"
            ""
        "#]].assert_debug_eq(&clean_inline_lints(""));
        expect![[r#"
            "a"
        "#]].assert_debug_eq(&clean_inline_lints("a"));
        expect![[r#"
            "a\n"
        "#]].assert_debug_eq(&clean_inline_lints("a\n"));
    }

    #[test]
    fn test_apply_inline_lints() {
        let src = "foo bar baz\nxxx yyy";
        let lints = [
            (
                TextRange::at(TextSize::of("foo "), TextSize::of("bar")),
                "bar lint",
            ),
            (
                TextRange::at(TextSize::of("foo "), TextSize::of("b")),
                "bar sub lint",
            ),
            (
                TextRange::at(TextSize::of("foo bar "), TextSize::of("baz")),
                "baz lint",
            ),
            (
                TextRange::at(TextSize::of("foo bar baz\nxxx "), TextSize::of("yyy")),
                "yyy lint",
            ),
        ].map(|(rng, lint)| (rng, lint.to_owned())).to_vec();
        expect![[r#"
            foo bar baz
             // ^ bar sub lint
             // ^^^ bar lint
                 // ^^^ baz lint
            xxx yyy
             // ^^^ yyy lint"#]].assert_eq(&apply_inline_lints(src, lints));
        expect![[r#"
            ""
        "#]].assert_debug_eq(&apply_inline_lints("", vec![]));
        expect![[r#"
            "a"
        "#]].assert_debug_eq(&apply_inline_lints("a", vec![]));
        expect![[r#"
            "a\n"
        "#]].assert_debug_eq(&apply_inline_lints("a\n", vec![]));
}
}
