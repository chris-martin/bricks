# Bricks syntax

## The `bricks-syntax` package

The `bricks-syntax` package is organized into two parts:

* `Bricks.Syntax.Abstract` - The "abstract syntax tree" (AST), which represents
  the general structure of the Bricks language.
* `Bricks.Syntax.Concrete` - The "concrete syntax tree" (also known as the
  "parse tree"); this is a more complicated structure which gets into the finer 
  details of the grammar, including whitespace, comments, and the several forms 
  of strings.

## Strings 

The Bricks language supports three forms of string styles: normie strings, prose
strings, and box strings. All three string styles support some form of
antiquotation.

### Normie strings

Normie strings begin and end with a double-quote character (`"`). The are based
on double-quoted strings in Nix and Haskell. They support the familiar escape
sequences like `\n` for newline. A string expression looks like this:

    "Hello"

Antiquotation look like this: `${` *expr* `}`. For example:

    "Hello, my name is ${name}."

### Prose strings

Prose strings are designed for writing paragraphs. The parser interprets the
contents of a prose string in the following way:

1. All leading and trailing whitespace is removed.
2. Each sequence of two or more newlines is interpreted as exactly two newlines.
3. Each other sequence of whitespace is interpreted as a single space character.

These rules give Bricks formatting tools the freedom to automatically wrap lines
of prose without altering their semantics.

The basic form of prose string begins and ends with two single-quote characters
(`''`):

    ''
      Hello
    ''

If the content of the string contains sequences of single-quote characters, you
can use additional single-quotes in the delimiter to avoid the need for 
escaping.

    '''
      This is important because that is 
      what ``quotation'' looks like in \LaTeX. 
    '''

Indeed, prose strings do not support any escape sequences at all.

Antiquotation in prose strings is disabled by default. If you want antiquotation 
in a prose string, you must specify which antiquote delimiters you want to use.
The complete list of delimiter options is:

`{}`, `${}`, `[]`, `$[]`, `<>`, `$<>`, `()`, `$()`, `«»`, `$«»`, `「」`, `$「」`

(`«` and `»` are *guillemets*, and `「` and `」` are *CJK brackets*.)

The antiquote delimiter goes after the opening `''`, on the same line. For 
example, the variable `url` is interpolated into the following string:

    '' {}
      <a href="{url}">Link</a>
    ''

### Box strings

Box strings are for situations where all whitespace is significant, such as for
code.

A box string starts with `┌`. Each subsequent line of the string must start with
`│`, except for the closing line which starts with `└`. The visual effect is
that of a box partially drawn around the string:

    ┌───
    │filter _pred []    = []
    │filter pred (x:xs)
    │  | pred x         = x : filter pred xs
    │  | otherwise      = filter pred xs
    └───

Because you may find these box-drawing characters difficult to type, we also
accept the following form which requires only ASCII characters:

    +---
    |filter _pred []    = []
    |filter pred (x:xs)
    |  | pred x         = x : filter pred xs
    |  | otherwise      = filter pred xs
    +---

We recommend typing in this format and using a formatting tool to convert the
ASCII characters to the fancier box-drawing characters.

Just like prose strings, box strings have no escape sequences, and antiquotation
must be explicitly enabled by specifying the antiquotation delimiters on the
opening line of the string.

    ┌─── <>
    │mkdir -p <cfg.outputDirectory>
    │chown <cfg.user>:<cfg.group> <cfg.outputDirectory> -R
    └───

## Inline comments

`--` or `──`

## Horizontal rules

A line which contains only whitespace and a sequence of three or more `-` or `─`
characters is  interpreted as a *horizontal rule*.
