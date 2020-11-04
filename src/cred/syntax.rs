/**
 * MIT License
 *
 * Copyright (c) 2020 Simon Mihevc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
extern crate regex;

use super::common::*;
use regex::Regex;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Clone, Debug)]
pub struct RegexGroup {
    style: FontStyle,
    regexes: Vec<Regex>,
}

impl RegexGroup {
    fn raw(style: FontStyle, regexes: Vec<&str>) -> Self {
        Self {
            style,
            regexes: str_to_regex(regexes[..].to_vec()),
        }
    }

    fn keyword(style: FontStyle, regexes: Vec<&str>) -> Self {
        Self {
            style,
            regexes: str_to_word_regex(regexes[..].to_vec()),
        }
    }
}

pub struct SyntaxMatch {
    pub start: usize,
    pub end: usize,
    pub style: FontStyle,
}

#[derive(Clone, Debug)]
pub struct Syntax {
    file_type: String,
    regex_groups: Vec<RegexGroup>,
}

impl Syntax {
    pub fn find_groups(&self, s: &str) -> Vec<SyntaxMatch> {
        let mut matches: Vec<SyntaxMatch> = Vec::new();
        for regex_group in &self.regex_groups[..] {
            for regex in &regex_group.regexes[..] {
                for m in regex.find_iter(s) {
                    matches.push(SyntaxMatch {
                        style: regex_group.style.clone(),
                        start: m.start(),
                        end: m.end(),
                    });
                }
            }
        }
        matches
    }
}

fn str_to_regex(v: Vec<&str>) -> Vec<Regex> {
    let mut result: Vec<Regex> = Vec::new();
    for item in v {
        let regex = Regex::new(item);
        if regex.is_err() {
            log::warn!("Failed compiling regex: {:?}", regex);
            continue;
        }
        result.push(regex.unwrap());
    }
    result
}

fn str_to_word_regex(v: Vec<&str>) -> Vec<Regex> {
    let mut result: Vec<Regex> = Vec::new();
    for item in v {
        let regex = Regex::new(&format!("(\\b|\\s){}(\\b|\\s)", item));
        if regex.is_err() {
            log::warn!("Failed compiling regex: {:?}", regex);
            continue;
        }
        result.push(regex.unwrap());
    }
    result
}

impl Syntax {
    fn none() -> Self {
        Self {
            file_type: String::from(""),
            regex_groups: vec![],
        }
    }

    fn python() -> Self {
        Self {
            file_type: String::from("py"),
            regex_groups: vec![
                RegexGroup::raw(COMMENT, vec![r"(^|\s|\w)#.*\b"]),
                RegexGroup::raw(COMMENT, vec!["(^|\\s|\\w)\"\"\"(?ms:.*?)\"\"\""]),
                RegexGroup::keyword(
                    KEYWORD_IMPORT,
                    vec!["def", "class", "lambda", "from", "import", "as"],
                ),
                RegexGroup::keyword(KEYWORD_TYPE, vec!["False", "True", "global", "nonlocal"]),
                RegexGroup::keyword(
                    KEYWORD_LOOP,
                    vec![
                        "for", "in", "while", "if", "is", "else", "break", "continue", "elif",
                        "None", "del", "try", "finally", "except", "and", "or", "not", "assert",
                        "pass", "with", "yield", "return", "raise",
                    ],
                ),
                RegexGroup::raw(INVERSE_STYLE, vec![r"[^\D]\d+[^\D]"]),
            ],
        }
    }

    fn rust() -> Self {
        Self {
            file_type: String::from("rs"),
            regex_groups: vec![
                RegexGroup::raw(
                    COMMENT,
                    vec![r"(\s|\w|^)/[*]{1}[*]?(?ms:.*?)[\*]{1}/(\b|\s)"],
                ),
                RegexGroup::raw(COMMENT, vec![r"(\s|\w|^)//.+\b"]),
                RegexGroup::keyword(
                    KEYWORD_IMPORT,
                    vec![
                        "pub", "struct", "fn", "extern", "crate", "enum", "trait", "use", "const",
                        "self", "&self", "Self", "mod", "let", "mut", "impl",
                    ],
                ),
                RegexGroup::keyword(
                    KEYWORD_TYPE,
                    vec![
                        "bool", "char", "f32", "f64", "i128", "i16", "i32", "i64", "i8", "isize",
                        "str", "u128", "u16", "u32", "u64", "u8", "usize", "String",
                    ],
                ),
                RegexGroup::keyword(
                    KEYWORD_LOOP,
                    vec![
                        "for", "in", "while", "loop", "if", "else", "break", "continue", "match",
                        "Some", "None",
                    ],
                ),
                RegexGroup::raw(STRING, vec!["[r]?\"(.*?)\""]),
                RegexGroup::raw(INVERSE_STYLE, vec![r"^\d+$"]),
            ],
        }
    }

    fn toml() -> Self {
        Self {
            file_type: String::from("toml"),
            regex_groups: vec![
                RegexGroup::raw(COMMENT, vec![r"(\s|\w|^)#.*"]),
                RegexGroup::raw(STRING, vec!["(\".*?\")"]),
            ],
        }
    }
}

pub struct SyntaxHighlight {
    syntaxes: HashMap<String, Syntax>,
}

impl SyntaxHighlight {
    pub fn new() -> Self {
        let syntaxes = vec![Syntax::none(), Syntax::rust(), Syntax::python(), Syntax::toml()];

        let mut syntax_lookup: HashMap<String, Syntax> = HashMap::new();
        for syntax in syntaxes {
            syntax_lookup.insert(syntax.file_type.clone(), syntax.clone());
        }
        Self {
            syntaxes: syntax_lookup,
        }
    }

    pub fn find_syntax(&self, pathbuf: PathBuf) -> Syntax {
        let no_syntax = Syntax::none();
        match pathbuf.extension() {
            Some(osstr) => {
                let extension = osstr.to_str();
                match extension {
                    Some(extension) => self.syntaxes.get(extension).unwrap_or(&no_syntax).clone(),
                    _ => no_syntax,
                }
            }
            _ => no_syntax,
        }
    }
}
