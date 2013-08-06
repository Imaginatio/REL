# Matchers

REL comes with some _matchers_ built-in for commonly needed entities like dates. _Matchers_ are `RE` expressions and corresponding extractors.

As the matchers collection will probably grow, they may come in a separate packaging one day (e.g. _rel-contrib_), but should keep their package and class names.

## Dates

The dates matchers provide regexes and extractors for dates, both numeric (1/23/12, 2012-01-23) and alphanumeric (january 23rd, 2012), for English and French, partial (with at least a month or a year) or full.

### Regexes

Regex in `rel.matchers.…` | Matches
----------------------|--------
`Date.FULL`           | `YMD` and `DMY` numerical formats
`Date.FULL_US`        | `MDY`, `YMD` and `DMY` numerical formats
`Date.NUMERIC`        | Numerical, including partial dates
`Date.NUMERIC_US`     | Numerical, including `MDY` and partials
`en.Date.ALPHA`       | English alphanumerical dates or partials
`en.Date.ALPHA_FULL`  | English alphanumerical dates
`en.Date.ALL`         | English alphanumerical or numerical dates or partials
`en.Date.ALL_FULL`    | English alphanumerical or numerical dates
`fr.Date.ALPHA`       | French alphanumerical dates or partials
`fr.Date.ALPHA_FULL`  | French alphanumerical dates
`fr.Date.ALL`         | French alphanumerical or numerical dates or partials
`fr.Date.ALL_FULL`    | French alphanumerical or numerical dates

### Extractors

Numerical dates may be ambiguous. For this reason, the date extractors will extract, for each match in the input string, a `List[DateExtractor.Result]`. Any additional disambiguation is left to client code.

`DateExtractor.Result` is a case class with three `Option[Int]`: `y`, `m`, `d`. A year may be on 2 or 4 digits, left for interpretation too. The `toString` method provides search-engine-friendly tokens: `1998-10-20` will yield `Y1998 Y10 M10 D20` (note the doubling of the year in 2 digits form too).

The extractors to use are:

- `matchers.DateExtractor` for numeric (full or partial, English or French)
- `matchers.en.FullDateExtractor` for `matchers.en.*_FULL`
- `matchers.en.DateExtractor` for `matchers.en.*`
- `matchers.fr.FullDateExtractor` for `matchers.fr.*_FULL`
- `matchers.fr.DateExtractor` for `matchers.fr.*`

To extract dates from a `String`:

```scala
import fr.splayce.rel.matchers
import matchers.DateExtractor.NUMERIC
import matchers.fr.{FullDateExtractor => FrDateExtr}

NUMERIC("2012-12-31").next // List(Y2012 Y12 M12 D31)
FrDateExtr("31 janvier 2013").next // List(Y2013 Y13 M01 D31)
```

We can also reuse those extractor in other extractors to get Lists of `DateExtractor.Result` directly form a `String`, `Regex.Match`, `MatchGroup`… `DateExtratorSpec` contains several examples and use cases.

## Utilities

The `matchers` package also provides a few utility functions to help build other matchers. The `escaped` and `unescaped` functions build a `RE` expression to match an escaped (resp. unescaped) sub-expression, i.e. preceded by an even (resp. odd) number of the escape string.