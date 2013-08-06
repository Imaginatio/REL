# Cleaners

## Usage

`Cleaner` is really just a case class around a `String => String` function. It is aimed to help pre-processing text before matching; its usage is completely optional. It also holds some utility methods to ease composing and instantiation.

You create a `Cleaner` simply by giving it a function:

```scala
val lineBreakNormalizer = Cleaner(_.replaceAllLiterally("\r\n", "\n");
```

There is a shorthand for regex replacement:

```scala
val stripMultipleDashes = Cleaner.regexReplaceAll("--+".r, "-")
```

A `Cleaner` extends `Function[String, String]`, you use it like any other function, either `someCleaner(someString)` or `someCleaner.apply(someString)`.

The most readable/familiar form of composing is using unix-like pipes, intuitively applied from left to right:

```scala
val myCleaner = lineBreakNormalizer | TrimFilter | LowerCaseFilter
```

The pros of heavy cleaning when you can afford it is to match more variations (accents, case sensitivity, double spaces…) with simpler (and possibly faster) regexes. The cons are an upfront performance cost (not necessarily worse than a more complex/permissive regex) and more importantly matching on an altered text, making it harder to locate matches in the original text. This can be addressed by `TrackString` (covered later in this chapter) but at an additional performance cost.


## Built-in Cleaners

In the built-in Cleaners, the naming convention follow these rules of thumb:

- `*Normalizer` normalizes variations of the same things
- `*Cleaner` cleans up information that is irrelevant for the task at hand
- `*Filter` transforms the text to prepare it for matching

The bundled Cleaners are:

Name                      | Usage
--------------------------|--------------------
`IdentityCleaner`         | Utility no-op Cleaner
`CamelCaseSplitFilter`    | Split CamelCase words; follows the the form `aBc` (lower-upper-lower): will split `someWords` but not `iOS` nor `VitaminC`
`LowerCaseFilter`         | Transform the text in lowercase;  a lowercase-only regexes will often outperform case-insensitive
`LineSeparatorNormalizer` | Normalize all Unicode line breaks and vertical tabs to ASCII new line `U+000A` / `\n`
`WhiteSpaceNormalizer`    | Normalize all Unicode spaces and horizontal tabs to ASCII spaces `U+0020`
`WhiteSpaceCleaner`       | Replace multiple instances of regular whitespaces (`\s+`) by a single space (**strip line breaks**)
`AllWhiteSpaceCleaner`    | Replace multiple instances of all Unicode whitespaces by a single space (**strip line breaks**)
`SingleQuoteNormalizer`   | Normalize frequent Unicode single quote / apostrophe variations (like prime of curved apostrophe) to ASCII straight apostrophe `U+0027` / `'`
`DoubleQuoteNormalizer`   | Normalize frequent Unicode double quote variations to ASCII quotation mark `U+0022` / `"`
`QuoteNormalizer`         | Combines `SingleQuoteNormalizer` and `DoubleQuoteNormalizer`
`DiacriticCleaner`        | Pseudo ASCII folding, remove [diacritical marks](http://en.wikipedia.org/wiki/Diacritic) (like accents) and some common Unicode variants on Latin characters
`FullwidthNormalizer`     | Normalize [CJK Fullwidth](http://en.wikipedia.org/wiki/Halfwidth_and_fullwidth_forms) Latin characters to their ASCII equivalents


## Create a new Cleaner

You can of course create your own Cleaners.

- If your cleaning operation can fit in a single regex replacement:

	```scala
	object HtmlTagCleaner extends Cleaner(
	  Cleaner.regexReplaceFirst("<html[^>]*+>(.*)</html>", "$1"))
	// or val htmlTagCleaner: Cleaner = Cleaner.regexReplaceFirst(…)
	```

- Same for multiple regex replacement:

	```scala
	object HtmlCommentsCleaner extends Cleaner(
	  Cleaner.regexReplaceAll("<!--(.*)-->", ""))
	```

- Otherwise, you can simply instantiate a Cleaner with your own `String => String` transformation.



## TrackStrings

TrackStrings are strings that can, to a certain extent, keep track of the shifts in positions. You pass Strings through Cleaners / regex replacement and remain able to get the position (or the best estimated range) in the original string of a [group of] character[s] that have moved in the resulting string:

```scala
import fr.splayce.rel.util.TrackString
val ts = TrackString("Test - OK - passed")
  .replaceAll(" - ".r, " ")  // "Test OK passed"
ts.srcPos(5, 7)   // Interval [7,9) was the original position of "OK"
ts.srcPos(8, 14)  // Interval [12,18) was the original position of "passed"
```

And Cleaners support TrackStrings, so this allows you to:

- Clean an input string
- Match it against a simplified regex (thanks to the cleaning)
- Know the position of the match _in the original uncleaned string_ (e.g. for highlighting matches, performing replacements, etc.)

```scala
import fr.splayce.rel.cleaners.CamelCaseSplitFilter
val os = "MySuperClass"
val ts = CamelCaseSplitFilter(TrackString(os))  // "My Super Class"
val m = "Super".r.findFirstMatchIn(ts.toString).get
val op = ts.srcPos(m.start, m.end)  // Interval [2,7)
val highlight = os.substring(0, op.start)
  + "<strong>" + os.substring(op.start, op.end) + "</strong>"
  + os.substring(op.end)
  // "My<strong>Super</strong>Class"
```

Please note that, while Cleaners made with `Cleaner.regexReplaceFirst` and `Cleaner.regexReplaceAll` automatically support position tracking, you will have to implement `apply: TrackString => TrackString` if you implement your own cleaners, e.g. by calling `TrackString.edit` (see the API doc). Otherwise, the TrackString will see your string transformation as one big replacement – i.e. it will tell you that the original position of any character is somewhere between the beginning and the end of your original string, which admittedly isn't of much help.