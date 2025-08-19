use std::ops::Range;

pub type ByteSpan = Range<usize>;

#[derive(Debug, Clone, Copy)]
pub enum ErrorKind {
    Lex,
    Parse,
    Type,
    Codegen,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub kind: ErrorKind,
    pub message: String,
    pub span: ByteSpan,
}

fn line_starts(src: &str) -> Vec<usize> {
    let mut v = vec![0];
    for (i, b) in src.bytes().enumerate() {
        if b == b'\n' {
            v.push(i + 1);
        }
    }
    v
}

fn offset_to_line_col(starts: &[usize], offset: usize) -> (usize, usize) {
    let line = match starts.binary_search(&offset) {
        Ok(i) => i,
        Err(i) => i.saturating_sub(1),
    };
    let col = offset.saturating_sub(starts[line]);
    (line, col)
}

pub fn print_diagnostic(file: &str, src: &str, d: &Diagnostic) {
    let starts = line_starts(src);
    let (line, col) = offset_to_line_col(&starts, d.span.start);
    let line_start = starts.get(line).copied().unwrap_or(0);
    let line_end = src[line_start..]
        .find('\n')
        .map(|i| line_start + i)
        .unwrap_or(src.len());
    let text = &src[line_start..line_end];

    let kind = match d.kind {
        ErrorKind::Lex => "lex",
        ErrorKind::Parse => "parse",
        ErrorKind::Type => "type",
        ErrorKind::Codegen => "codegen",
    };

    eprintln!(
        "{}:{}:{}: {} error: {}",
        file,
        line + 1,
        col + 1,
        kind,
        d.message
    );

    eprintln!("{:>4} | {}", line + 1, text);

    // underline span (clamped to this line)
    let caret_start = d.span.start.saturating_sub(line_start);
    let caret_end = d.span.end.min(line_end);
    let caret_len = caret_end.saturating_sub(d.span.start).max(1);
    eprintln!(
        "     | {}{}",
        " ".repeat(caret_start),
        "^".repeat(caret_len)
    );
}
