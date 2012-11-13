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

These value are `RE` objects (trees/subtrees), which can be converted to `scala.util.matching.Regex` instances either implicitly (by importing `REL.Implicits._`) or explicitly (via the `.r` method).

The embedded [Date regexes](https://github.com/Imaginatio/REL/blob/master/src/main/scala/matchers/Date.scala) and [extractors](https://github.com/Imaginatio/REL/blob/master/src/main/scala/matchers/DateExtractor.scala) will give you more complete examples, matching several date formats at once with little prior knowledge.

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
    - Unnamed: `a.g` → `(a)` (a unique group name is generated internally)
    - Non-capturing: `a.ncg` → `(?:a)` or the short syntax `a.%`
    - [Atomic](http://www.regular-expressions.info/atomic.html): `a.ag` → `(?>a)` or the short syntax `a.?>`
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

### Exporting regexes (and other regex flavors)

The `.r` method on any `RE` (sub)tree returns a compiled `scala.util.matching.Regex`. The `.toString` method returns the source pattern (equivalent to `.r.toString`, so the pattern is verified).

For other regex flavors, a translation mechanism is provided: you may subclass `Flavor`, which exposes a `.express(re: RE)` method, returning a `Tuple[String, List[String]]`. The first element is the translated regex string, the second is a list of the group names (in order of appearance), allowing you to perform a mapping to capturing group indexes (like Scala does) if needed. A subclass of `Flavor` should override `.translate(re: RE)`, using pattern matching to recursively translate Java regex subtree with matching subtree in the destination regex Flavor. It should call `super.translate` in the default case to ensure proper recusion.

An example of translation into [.NET-flavored regex](http://www.regular-expressions.info/dotnet.html) is provided ([`DotNETFlavor`](https://github.com/Imaginatio/REL/blob/master/src/main/scala/flavors/DotNETFlavor.scala)), that

- translates `\w` to `[a-zA-Z0-9_]` (as .NET's `\w` covers UTF-8 letters including accented, while Java's covers only ASCII)
- turns any possessive quantifier into a greedy quantifier wrapped in an atomic group (which is a longer equivalent)
- inlines named groups and their references into the .NET `(?<name>expr)` syntax

[Regular-expression.info](http://www.regular-expressions.info)'s [regex flavors comparison chart](http://www.regular-expressions.info/refflavors.html) may be of use when writing a translation.


## TODO

- Core
    - Add missing short notation for non-greedy RepMode in numbered Rep (e.g. `...(0, 3, Reluctant)`)
    - Add character range support (at DSL level), with inversion (`[^...]`)
    - Shortcuts for `^` and `$` (beware `^` is currently used as exactly-N repeater operator)
    - Add a recursive (sub)tree rewriter, that would allow for instance to rename capturing groups in a second occurrence of a subtree. The reverse operation could be done by a decorator for the `Extractor` trait.
    - Consider using `'symbols` for group names
    - Add options to easily embed match flags`(?idmsux-idmsux)` when generating regex
    - Parse \[and limit] regex strings inputted to REL, producing REL-only expression trees, thus eliminating some known issues (see below) and opening some possibilities (e.g. generating sample matching strings)
- Matchers
    - date: consider extracting incorrect dates (like feb. 31st) with some flag
    - date: also export a date regex that will only accept full forms (with mandatory day, month and year) while keeping the group names so that the DateExtractor still works
- Utils
    - Generate sample strings that match a regex (e.g. with [Xeger](http://code.google.com/p/xeger/))
    - Source generation or compiler plugin to enable REL independance \[at runtime]
    - Binary tool that would take a REL file, compile it and produce regexes in several flavors / programming langagues
- Documentation
    - Document cleaners and extractors


## Known issues

The string primitives are not parsed, so

- Any group you pass inside those strings won't be taken into account by REL when the final regex is generated. The following groups and back-references will be shifted so the resulting regex will most probably be incorrect.
- You still need to escape your expressions to match regex-significant characters like `+`, `?` or `(`, even in `RECst` (pending update on this point).
- Any regex you pass as a string literal will be kept as-is when translated into different flavors. For instance, the dot `.` does not have the same meaning in a JavaScript regex where is does not match a new line `\n`.

Java does not support named capturing groups, and Scala only emulates them, mapping a list of names given at the compilation of the Regex against the indexes of the capturing groups. Thus, you cannot have multiple instances of the same group name (in practice, doing this seems to always refer to the last occurrence of the identically-named groups).

Besides, JavaScript regexes are very limited and work a bit differently. In [JavaScript flavor](https://github.com/Imaginatio/REL/blob/master/src/main/scala/flavors/JavaScriptFlavor.scala)

- `WordBoundary`/`\b` is kept as-is, but will not have exactly the same semantic because of the lack of Unicode support in JavaScript regex flavor. For instance, in `"fiancé"`, Javascript sees `"\bfianc\bé" where most other flavors see `"\bfiancé\b"`. Same goes for `NotWordBoundary`/`\B`.
- `InputBeginning` and `InputEnd` are translated to `LineBeginning` and `LineEnd`, but this is only correct if the `m` (multiline) flag is off.

In [.NET flavor](https://github.com/Imaginatio/REL/blob/master/src/main/scala/flavors/DotNETFlavor.scala), the group names are not guaranteed to be valid.


## Usage and downloads

- download the [source from github](https://github.com/Imaginatio/REL) and build the library with SBT
- download the [latest binary release](https://github.com/Imaginatio/REL/downloads)
- use [our public Maven repository](https://github.com/Imaginatio/Maven-repository/)


## License

Copyright &copy; 2012 Imaginatio SAS

REL is released under the [MIT License](http://www.opensource.org/licenses/MIT)


## Authors

REL was developped by [Adrien Lavoillotte](http://instanceof.me/) ([@streetpc](https://github.com/streetpc)) and Julien Martin for project [Splayce](http://splayce.com) at [Imaginatio](http://imaginatio.fr)
