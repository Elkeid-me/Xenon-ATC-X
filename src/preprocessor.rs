enum State {
    Code,
    CodeWithBackSlash,
    CodeWithSlash,
    StringLiteral,
    StringLiteralWithEscape,
    Comment,
    CxxComment,
    CxxCommentWithBackSlash,
    CommentWithStar,
}

use State::*;

fn code_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '/' => (CodeWithSlash, None, None),
        '\\' => (CodeWithBackSlash, None, None),
        '"' => (StringLiteral, Some(c), None),
        _ => (Code, Some(c), None),
    }
}
fn code_with_back_slash_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\\' => (CodeWithBackSlash, Some('\\'), None),
        '\n' => (Code, None, None),
        _ => (Code, Some('\\'), Some(c)),
    }
}
fn code_with_slash_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '/' => (CxxComment, None, None),
        '*' => (Comment, None, None),
        _ => (Code, Some('/'), Some(c)),
    }
}
fn string_literal_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\\' => (StringLiteralWithEscape, None, None),
        '"' => (Code, Some(c), None),
        _ => (StringLiteral, Some(c), None),
    }
}
fn string_literal_with_escape_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\n' => (StringLiteral, None, None),
        _ => (StringLiteral, Some('\\'), Some(c)),
    }
}
fn comment_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '*' => (CommentWithStar, None, None),
        _ => (Comment, None, None),
    }
}
fn cxx_comment_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\n' => (Code, Some(c), None),
        '\\' => (CxxCommentWithBackSlash, None, None),
        _ => (CxxComment, None, None),
    }
}
fn cxx_comment_with_back_slash_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '\\' => (CxxCommentWithBackSlash, None, None),
        _ => (CxxComment, None, None),
    }
}
fn comment_with_star_fun(c: char) -> (State, Option<char>, Option<char>) {
    match c {
        '/' => (Code, Some(' '), None),
        '*' => (CommentWithStar, None, None),
        _ => (Comment, None, None),
    }
}

pub fn preprocess(code: &str) -> String {
    let mut new_code = String::new();
    let mut state = Code;
    let mut new_char_1: Option<char>;
    let mut new_char_2: Option<char>;
    for c in code.chars() {
        (state, new_char_1, new_char_2) = match state {
            Code => code_fun(c),
            CodeWithSlash => code_with_slash_fun(c),
            CodeWithBackSlash => code_with_back_slash_fun(c),
            Comment => comment_fun(c),
            CxxComment => cxx_comment_fun(c),
            CommentWithStar => comment_with_star_fun(c),
            StringLiteral => string_literal_fun(c),
            StringLiteralWithEscape => string_literal_with_escape_fun(c),
            CxxCommentWithBackSlash => cxx_comment_with_back_slash_fun(c),
        };
        if let Some(new_char) = new_char_1 {
            new_code.push(new_char);
        }
        if let Some(new_char) = new_char_2 {
            new_code.push(new_char);
        }
    }
    new_code
}
