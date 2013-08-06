# Limitations & Known Issues


## Versioning

REL version number follows the [Semantic Versioning 2.0 Specification](http://semver.org/). In the current early stage of development, the API is still unstable and backward compatibility may break.
As an additional rule, in version `0.Y.Z`, a `Z`-only version change is expected to be backward compatible with previous `0.Y.*` versions. But a `Y` version change potentially breaks backward compatibility.


## DSL

There is no representation in the DSL for specific character ranges nor raw strings.

The **string primitives are not parsed** (use `esc(str)` to escape a string that should be matched literally). Hence:

- Any capturing group you pass inside those strings are not taken into account by REL when the final regex is generated. The following groups and back-references will be shifted so the resulting regex will most probably be incorrect.
- You still need to escape your expressions to match literally characters that are regex-significant like `+`, `?` or `(`, even in `RECst`. Use `esc(str)` to escape the whole string.
- Any regex you pass as a string is kept as-is when translated into different flavors. For instance, the `\w` passed in a string (as opposed to used with `Word`/`μ`) will not be translated by the `DotNETFlavor`.


## Flavors

The Group names are checked but not inlined silently if they fail the validation, or if they are duplicated when the flavor requires unicity.

`\uXXXX` is not supported by PCRE, yet not translated by `PCREFlavor` so far.

JavaScript regexes are quite limited and work a bit differently. In JavaScript flavor:

- `WordBoundary`/`\b` is kept as-is, but will not have exactly the same semantic because of the lack of Unicode support in JavaScript regex flavor. For instance, in `"fiancé"`, Javascript sees `"\bfianc\bé"` where most other flavors see `"\bfiancé\b"`. Same goes for `NotWordBoundary`/`\B`.
- `InputBegin` (`^^`) and `InputEnd` (`$$`) are translated to `LineBegin` (`^`) and `LineEnd` (`$`), but this is only correct if the `m` (multiline) flag is off.


## Cleaners

Not all Unicode ligatures and variations are known to `DiacriticCleaner`, for example:

- Enclosed Alphanumeric Supplement: `U+1F100-U+1F1FF` (Unicode 6.1)
- CJK Compatibility: `U+3300-U+33FF` (Unicode 6.0)
- Latin Extended-D `U+A720-U+A7FF` (Unicode 5.1 to 6.1)


## TrackString

Regex replacement in `TrackString` do not support Java 7 embedded group names, which are not accessible in Scala's `Match` yet. It will use Scala group names instead (inconsistent with `String#replaceAll`).

`TrackString` cannot track intertwined/reordered replacements, i.e. you can only track `abc` => `bca` as a single group (as opposed to three reordered groups). If out-of-order `Repl`/`Subst` are introduced, `srcPos` will most probably yield incorrect results.


## TODO

The following would be useful:

- Core
    - Add character range support (at DSL level), with inversion (`[^...]`)
    - Compatibility with Scala Parsers?
    - Consider using `'symbols` for group names
    - Java 6/7 flavors: detect & fail on unbounded repeats in LookBehind ?
    - Parse \[and limit] regex strings inputted to REL, producing REL-only expression trees, thus eliminating some known issues (see below) and opening some possibilities (e.g. generating sample matching strings)
- Matchers
    - date: consider extracting incorrect dates (like feb. 31st) with some flag
- Utils
    - Generate sample strings that match a regex (e.g. with [Xeger](http://code.google.com/p/xeger/))
    - Source generation or compiler plugin to enable REL independence \[at runtime]
    - Binary tool that would take a REL file, compile it and produce regexes in several flavors / programming languages
