# Almu575

This is a domain specific markup language intended to make it easier
to type algorithms.

The name comes from **Al**gorithm **m**ark**u**p language for CIS **575**.

## Language Design

The language is a designed around the idea of environments and
symbols.
An environment does something to the internal text, for example `\bold [ text ]` will make the `text` bold.
A symbol is something that is intended to be replaced by a string.
For example, `\assign` will become `⭠`.

## Environments

The following environments are available for use.
They must be prefaced with a
`\` to be used.
The text that the interact with should be contained in a pair of square braces `[ ]` folowing the environment name.
Whitespace between the environment name and braces will be ignored.

* `title`: Renders the text as the document title. Limit 1 per document.
* `author`: Renders the text as the author of the document.
    Limit 1 per document.
* `date`: If the text block is empty it will produce todays data, otherwise it
    will render the provided text as the date at the beginning of the document.
* `section`: Generates a label for a section.
* `subsection`: Generates a label for a subsection
* `code`: Leaves the code inside untouched except for all `\`'s must be written
    as `\\` because `\` is still used for symbols. So `\assign` will still
    become `⭠` even in a `code` block.
* `displaymath`: Renders math using Mathjax (latex-style equations) as a
        standalone equation.
    * Please note that symbols cannot be used in a `displaymath` environment as
        they can conflict with the math symbols.
* `math`:  Renders math using Mathjax (latex-style equations) inline.
    * Please note that symbols cannot be used in a `math` environment as they
        can conflict with the math symbols.

## Symbols

The following symbols are provided by default.

### Equalities

* lt : <
* le : ≤
* gt : >
* ge : ≥
* neq : ≠

### Logic

* forall : ∀
* exists : ∃
* land : ∧

### Sets

* in : ∈
* notin : ∉

### Misc

* assign : ⭠

## Symbol Extensions

Additional labels may be specified

## Example Documents

Example files may be found in ...

## Math

If you need to type a symbol in a math environment that you don't
know. Try [Detexify](http://detexify.kirelabs.org/classify.html),
you can draw the symbol and it will figure out the symbols that
look the most like it.
