# REL, a Regular Expression composition Library

REL is a small utility Scala library for people dealing with complex, modular regular expressions. It defines a DSL with most of the operators you already know and love. This allows you to isolate portions of your regex for easier testing and reuse.

Consider the following YYYY-MM-DD date regex: `^(?:19|20)\d\d([- /.])(?:0[1-9]|1[012])\1(?:0[1-9]|[12]\d|3[01])$`. It is a bit more readable and reusable expressed like this:

```scala
import fr.splayce.REL._
import Implicits._

val sep     = "[- /.]" \ "sep"            // group named "sep"
val year    = ("19" | "20") ~ """\d\d"""  // ~ is concatenation
val month   = "0[1-9]" | "1[012]"
val day     = "0[1-9]" | "[12]\\d" | "3[01]"
val dateYMD = "^" ~ year  ~ sep ~ month ~ !sep ~ day  ~ "$"
val dateMDY = "^" ~ month ~ sep ~ day   ~ !sep ~ year ~ "$"
```

These value are `RE` objects, which can be converted to `scala.util.matching.Regex` instances either implicitly (by importing `REL.Implicits._` or explicitly (via the `.r` method).

The embedded [Date regexes](./REL/src/main/scala/matchers/Date.scala) and [extractors](./REL/src/main/scala/matchers/DateExtractor.scala) will give you more complete examples, matching several date formats at once with little prior knowledge.

### Supported opperators

> Examples are noted `DSL expression` → `resulting regex`. They assume:
> ```scala
import fr.splayce.REL._
import Implicits._
val a = RE("a")
val b = RE("b")
```

- Concatenation:
    - Protected: `a ~ b` → `(?:a)(?:b)`
    - Unprotected: `a - b` → `ab`
- Alternative: `a | b` → `a|b`
- Option:
    - [greedy](http://www.regular-expressions.info/repeat.html#greedy) `a.?` → `(?:a)?` ; you can also skip the dot `a ?` but the former has clearer priority in a complex expression
    - [reluctant / lazy](http://www.regular-expressions.info/repeat.html#lazy): `a.??` → `(?:a)??`
    - [possessive](http://www.regular-expressions.info/possessive.html): `a.?+` → `(?:a)?+`
- Repeat:
    - At least one:
        - greedy: `a.+ ` → `(?:a)+`
        - reluctant: `a.+? ` → `(?:a)+?`
        - possessive: `a.++ ` → `(?:a)++`
    - Any number:
        - greedy: `a.* ` → `(?:a)*`
        - reluctant: `a.*? ` → `(?:a)*?`
        - possessive: `a.*+ ` → `(?:a)*+`
    - In range: `a(1,3)` or `a(1 to 3)` → `(?:a){1,}`
    - At most: `a(0,3)` → `(?:a){0,3}` (duh)
    - At least: `a(3)` → `(?:a){3,}`
    - Exactly: `a^3` → `(?:a){3}`
- Lookaround:
    - Lookahead: `a.>?` → `(?=a)`
    - Lookbehind: `a.<?` → `(?<=a)`
    - Negative lookahead: `a.>!` → `(?!a)`
    - Negative lookbehind: `a.<!` → `(?<!a)`
- Grouping:
    - Named: `a \ "group_a"` → `(a)`; the name `group_a` will be passed to the `Regex` constructor,  queryable on corresponding `Match`es
    - Unnamed: `a.g` → `(a)`
    - Non-capturing: `a.ncg` → `(?:a)` or the short syntax `a.%`
- Back-reference: `!g` will insert a backreference on group `g`; e.g. `val g = (a|b).g; g - a - !g` → `(a|b)a\1`

### Constants

A few "constants" (sub-expressions with no repetitions, capturing groups, or unprotected alternatives) are also pre-defined. Some of them have a UTF-8 Greek symbol alias for conciseness (import `REL.Symbols._` to use them), uppercase for negation. You can add your own by instancing case class `RECst(expr)`

- `Epsilon` or `ε` is empty string
- `AlphaLower` → `[a-z]`, `AlphaUpper` → `[A-Z]`
- `Alpha` or `α` → `[a-zA-Z]`, `NotAlpha` or `Α`* → `[^a-zA-Z]`
- `LetterLower` → `\p{Ll}`, `LetterUpper` → `\p{Lu}` (unicode letters, including)
- `Letter` or `λ` → `\p{L}`, `NotLetter` or `Λ` → `\P{L}`
- `Digit` or `δ` → `\d`, `NotDigit` or `Δ` → `\D`
- `WhiteSpace` or `σ` → `\s`, `NotWhiteSpace` or `Σ` → `\S`
- `Word` or `μ` → `\w` (`Alpha` or `_`), `NotWord` or `Μ`* → `\W`
- `WordBoundary` or `ß` → `\b`, `NotWordBoundary` or `Β`* → `\B`
- `LineBeginning` → `^`, `LineEnd` → `$`
- `InputBeginning` → `\A`, `InputEnd` → `\z`

_\* Those are uppercase `α`/`ß`/`μ`, not latin `A`/`B`/`M`_


## TODO

- Core
    - Add missing short notation for non-greedy RepMode in numbered Rep
    - Add [atomic grouping](http://www.regular-expressions.info/atomic.html) support
    - Add character range support (at DSL level), with inversion (`[^...]`)
    - Shortcuts for `^` and `$` (beware `^` is currently used as exactly-N repeater operator)
    - Consider using `'symbols` for group names
- Matchers
    - date: consider extracting incorrect dates (like feb. 31st) with some flag
- Utils
    - Source generation or compiler plugin to enable REL independance \[at runtime]


## Known issues

The string primitives are not parsed, so any group you pass inside those strings won't be taken into account by REL when the final regex is generated. The following groups and back-references will be shifted so the resulting regex will most probably be incorrect.


## License

Copyright &copy; 2012 Imaginatio SAS

REL is released under the [MIT License](http://www.opensource.org/licenses/MIT)


## Authors

REL was developped by [Adrien Lavoillotte](http://instanceof.me/) ([@streetpc](https://github.com/streetpc)) and Julien Martin for project [Splayce](http://splayce.com) at [Imaginatio](http://imaginatio.fr)
