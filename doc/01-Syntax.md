# DSL Syntax

Some examples are noted `DSL expression` → `resulting regex`.  
All assume:

```scala
import fr.splayce.rel._
import Implicits._
val a = RE("aa")
val b = RE("bb")
```

## Operators

### Binary operators

Operation                | REL Syntax | RE Output
----------------------------|---------|---------------
Alternative                 | `a | b` | `aa|bb`
Concatenation (protected)   | `a ~ b` | `(?:aa)(?:bb)`
Concatenation (unprotected) | `a - b` | `aabb`

Generally speaking, you should start with protected concatenation. It is harder to read once serialized, but it is far safer from unwanted side-effects when reusing regex parts.

### Quantifiers / repeaters

When used in the table below, the dot syntax `a.?` is recommended for clearer priority.

Quantifier | [Greedy](http://www.regular-expressions.info/repeat.html#greedy) | [Reluctant / Lazy](http://www.regular-expressions.info/repeat.html#lazy) | [Possessive](http://www.regular-expressions.info/possessive.html) | Output for (greedy)
---------|---------|-------------|----------|---------
Option   | `a.?`   | `a.??`      | `a.?+`   | `(?:aa)?`
≥ 1      | `a.+`   | `a.+?`      | `a.++`   | `(?:aa)+`
≥ 0      | `a.\*`   | `a.\*?`      | `a.\*+`   | `(?:aa)\*`
At most  | `a < 3` | `a.<?(3)`\* | `a <+ 3` | `(?:aa){0,3}`
At least | `a > 3` | `a >? 3`    | `a >+ 3` | `(?:aa){3,}`
In range | `a(1, 3)`, `a{1 to 3}` or `a{1 -> 3}` | `a(1, 3, Reluctant)` | `a(1, 3, Possessive)` | `(?:aa){1,3}`
Exactly  | `a{3}` or `a(3)` | _N/A_ | _N/A_ | `(?:aa){3}`

_\* For reluctant at-most repeater, dotted form `a.<?(3)` is mandatory, standalone `<?` being syntactically significant in Scala (`XMLSTART`)._

### Look-around

                     | Prefixed form | Dotted form | Output
---------------------|---------------|-------------|----------
Look-ahead           | `?=(a)`       | `a.?=`      | `(?=aa)`
Look-behind          | `?<=(a)`      | `a.?<=`     | `(?<=aa)`
Negative look-ahead  | `?!(a)`       | `a.?!`      | `(?!aa)`
Negative look-behind | `?<!(a)`      | `a.?<!`     | `(?<!aa)`

### Grouping

Type                      | REL Syntax       | Output
--------------------------|------------------|----------
Named capturing           | `a \ "group_a"`  | `(aa)`. 
Unnamed capturing \*      | `a.g`            | `(aa)`
Back-reference            | `g!`             | `\1`\*\*
Non-capturing             | `a.ncg` or `a.%` | `(?:aa)` 
Non-capturing, with flags | `a.ncg("i-d")` or `"i-d" ?: a` | `(?i-d:aa)` 
[Atomic](http://www.regular-expressions.info/atomic.html) | `a.ag`, `?>(a)` or `a.?>` | `(?>aa)`

_\* A unique group name is generated internally._  
_\*\* Back-reference on most recent (i.e. rightmost previous) group `g`. `val g = (a|b).g; g - a - !g` → `(aa|bb)aa\1`_

In a named capturing group, the name `group_a` will be passed to the [`Regex`](http://www.scala-lang.org/api/current/index.html#scala.util.matching.Regex) constructor, and queryable on corresponding [`Match`](http://www.scala-lang.org/api/current/index.html#scala.util.matching.Regex$$Match)es. If you export the regex to a flavor that supports inline embedding of capturing group names (like Java 7 or .NET), the name will be included in the output: `(?<group_a>aa)`.

In non-capturing groups, REL tries not to uselessly wrap non-breaking entities — like single characters (`a`, `\u00F0`), character classes (`\w`, `[^a-z]`, `\p{Lu}`), other groups — in order to produce ever-so-slightly less unreadable output. Non-capturing groups with flags are combined when nested, giving priority to innermost flags: `a.ncg("-d").ncg("id")` → `(?i-d:aa)`.


## Constants

A few "constants" (expression terms with no repetitions, capturing groups, or unprotected alternatives) are also predefined. Some of them have a UTF-8 Greek symbol alias for conciseness (import `rel.Symbols._` to use them), uppercase for negation. You can add your own by instancing case class `RECst(expr)`.

Object name      | Symbol | Output / Matches
-----------------|--------|----------------
`Epsilon`        | `ε`    | Empty string
`Dot`            | `τ`    | `.`
`MLDot`          | `ττ`   | `[\s\S]` (will match any char, including line terminators, even when the [`DOTALL`](http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#DOTALL) or [`MULTILINE`](http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#MULTILINE) modes are disabled)
`LineTerminator` | `Τ`\*  | `(?:\r\n?|[\u000A-\u000C\u0085\u2028\u2029])` ([line terminators](http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#lt), PCRE/Perl's `\R`)
`AlphaLower`     | _none_ | `[a-z]`
`AlphaUpper`     | _none_ | `[A-Z]`
`Alpha`          | `α`    | `[a-zA-Z]`
`NotAlpha`       | `Α`\*  | `[^a-zA-Z]`
`Letter`         | `λ`    | `\p{L}` (unicode letters, including diacritics)
`NotLetter`      | `Λ`    | `\P{L}`
`LetterLower`    | _none_ | `\p{Ll}`
`LetterUpper`    | _none_ | `\p{Lu}`
`Digit`          | `δ`    | `\d`
`NotDigit`       | `Δ`    | `\D`
`WhiteSpace`     | `σ`    | `\s`
`NotWhiteSpace`  | `Σ`    | `\S`
`Word`           | `μ`    | `\w` (`Alpha` or `_`)
`NotWord`        | `Μ`\*  | `\W`
`WordBoundary`   | `ß`    | `\b`
`NotWordBoundary` | `Β`\* | `\B`
`LineBegin`      | `^`    | `^`
`LineEnd`        | `$`    | `$`
`InputBegin`     | `^^`   | `\A`
`InputEnd`       | `$$`   | `\z`

_\* Those are uppercase `α`/`ß`/`μ`/`τ`, not latin `A`/`B`/`M`/`T`_
