use std::{borrow::Cow, marker::PhantomData};

#[derive(Clone, PartialEq, Eq)]

pub struct Word<'a>(Cow<'a, str>);

#[derive(Clone, PartialEq, Eq)]

pub struct Words<'a>(Vec<Word<'a>>);

#[derive(Clone, PartialEq, Eq)]

pub struct Cased<What, Marker> {
    what: What,

    _marker: PhantomData<Marker>,
}

pub enum CaseKind {
    SnakeCase,

    // CamelCase,
    PascalCase,
    // ScreamingCase,

    // KebabCase,
}

macro_rules! declare_case_marker {
    ($marker:ident, $kind:ident) => {
        pub struct $marker {}

        impl $marker {
            fn kind(&self) -> CaseKind {
                CaseKind::$kind
            }
        }
    };
}

declare_case_marker! {SnakeCase, SnakeCase}

// declare_case_marker! {CamelCase, CamelCase}

declare_case_marker! {PascalCase, PascalCase}

// declare_case_marker! {ScreamingCase, ScreamingCase}

// TODO: add some kind of fn guess_case() -> Vec<CaseKind>

impl From<String> for Word<'static> {
    fn from(string: String) -> Self {
        assert!(string.len() > 0);

        Word(Cow::from(string))
    }
}

impl<'a> From<&'a str> for Word<'a> {
    fn from(string: &'a str) -> Self {
        assert!(string.len() > 0);

        Word(Cow::from(string))
    }
}

impl<What, Marker> Cased<What, Marker> {
    fn case(what: What) -> Self {
        Cased {
            what,

            _marker: PhantomData,
        }
    }

    fn unwrap(self) -> What {
        self.what
    }
}

impl<Marker> From<Cased<String, Marker>> for String {
    fn from(value: Cased<String, Marker>) -> Self {
        value.unwrap()
    }
}

impl<'a, Marker> From<Cased<&'a str, Marker>> for &'a str {
    fn from(value: Cased<&'a str, Marker>) -> Self {
        value.unwrap()
    }
}

pub trait Case: Sized {
    fn case_word<'a, W: Into<Word<'a>>>(word: W) -> Cased<Word<'a>, Self>;

    fn case_words<'a, Ws: Into<Words<'a>>>(words: Ws) -> Cased<Words<'a>, Self> {
        Cased::case(Words(
            Words::from(words.into())
                .0
                .into_iter()
                .map(|word| Self::case_word(word).unwrap())
                .collect(),
        ))
    }

    // fn to_words_strict<'a>(string: &'a str) -> Option<Words<'a>>;

    // fn to_words<'a>(string: &'a str) -> Words<'a>;

    fn from_words<'a>(words: Cased<Words<'a>, Self>) -> Cow<'a, str>;
}

impl<'a> Word<'a> {
    pub fn new<S: Into<Cow<'a, str>>>(string: S) -> Self {
        Word(string.into())
    }
}

impl<'a> Words<'a> {
    pub fn empty() -> Self {
        Words(Vec::new())
    }

    pub fn append(&mut self, word: Word<'a>) {
        self.0.push(word);
    }

    pub fn case<C: Case>(self) -> Cased<Words<'a>, C> {
        C::case_words(self)
    }

    pub fn join(self, separator: &'a str) -> Cow<'static, str> {
        Cow::from(
            self.0
                .into_iter()
                // needed because of https://github.com/rust-lang/rust/issues/27747#issuecomment-1055635718
                .map(|word| word.0)
                .collect::<Vec<_>>()
                .join(separator),
        )
    }
}

impl<'a, C: Case> Cased<Words<'a>, C> {
    pub fn append_cased(&mut self, word: Cased<Word<'a>, C>) {
        self.what.0.push(word.unwrap())
    }

    pub fn format(self) -> Cow<'a, str> {
        C::from_words(self)
    }
}

// TODO: it's quite simple parsing

pub fn to_words<'a>(string: &'a str) -> Words<'a> {
    let mut words = Words::empty();

    let mut iter = string.char_indices();

    let (mut start, mut prev) = match iter.next() {
        Some((ind, c)) => (ind, c),

        None => return words,
    };

    for (i, c) in iter {
        if c.is_uppercase() && !prev.is_uppercase() {
            words.append(Word::new(&string[start..i]));

            start = i;
        } else if c == '_' || c == '-' {
            if start < i {
                words.append(Word::new(&string[start..i]));
            }

            start = i + c.len_utf8();
        }

        prev = c;
    }

    if start < string.len() {
        words.append(Word::new(&string[start..]));
    }

    words
}

impl Case for SnakeCase {
    fn case_word<'a, W: Into<Word<'a>>>(word: W) -> Cased<Word<'a>, Self> {
        Cased::case(Word::from(word.into()).0.to_lowercase().into())
    }

    fn from_words<'a>(words: Cased<Words<'a>, Self>) -> Cow<'a, str> {
        let mut words = words.unwrap();

        assert!(words.0.len() > 0);

        if words.0.len() == 1 {
            words.0.remove(0).0
        } else {
            words.join("_")
        }
    }
}

impl Case for PascalCase {
    fn case_word<'a, W: Into<Word<'a>>>(word: W) -> Cased<Word<'a>, Self> {
        let word = Word::from(word.into());

        let mut chars = word.0.chars();

        let first = chars.next().expect("word expected to be non-empty");

        if first.is_lowercase() {
            Cased::case(Word(Cow::from(
                first.to_uppercase().chain(chars).collect::<String>(),
            )))
        } else {
            Cased::case(word)
        }
    }

    fn from_words<'a>(words: Cased<Words<'a>, Self>) -> Cow<'a, str> {
        let mut words = words.unwrap();

        assert!(words.0.len() > 0);

        if words.0.len() == 1 {
            words.0.remove(0).0
        } else {
            words.join("")
        }
    }
}

mod tests {

    use super::*;

    #[test]

    fn test() {
        let words = to_words("kekLol");

        assert_eq!(words.clone().case::<PascalCase>().format(), "KekLol");

        assert_eq!(words.case::<SnakeCase>().format(), "kek_lol");
    }
}
