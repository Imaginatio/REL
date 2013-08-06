---
out: index.html
---

# REL

**A Regular Expression composition Library**

REL is a small utility Scala library for people dealing with complex, modular regular expressions. It defines a DSL with most of the operators you already know and love. This allows you to isolate portions of your regex for easier testing and reuse.

Consider the following YYYY-MM-DD date regex: `^(?:19|20)\d\d([- /.])(?:0[1-9]|1[012])\1(?:0[1-9]|[12]\d|3[01])$`. It is a bit more readable and reusable expressed like this:

```scala
import fr.splayce.rel._
import Implicits._

val sep     = "[- /.]" \ "sep"            // group named "sep"
val year    = ("19" | "20") ~ """\d\d"""  // ~ is concatenation
val month   = "0[1-9]" | "1[012]"
val day     = "0[1-9]" | "[12]\\d" | "3[01]"
val dateYMD = ^ ~ year  ~ sep ~ month ~ !sep ~ day  ~ $
val dateMDY = ^ ~ month ~ sep ~ day   ~ !sep ~ year ~ $
```

These values are `RE` objects (also named _terms_ or _trees_/_subtrees_), which can be converted to `scala.util.matching.Regex` instances either implicitly (by importing `rel.Implicits._`) or explicitly (via the `.r` method).

The embedded [Date regexes](https://github.com/Imaginatio/REL/blob/master/src/main/scala/matchers/Date.scala) and [extractors](https://github.com/Imaginatio/REL/blob/master/src/main/scala/matchers/DateExtractor.scala) will give you more complete examples, matching several date formats at once with little prior knowledge.

## Usage and downloads

- download the [source from github](https://github.com/Imaginatio/REL) and build the library with SBT
- download the [latest binary release](https://github.com/Imaginatio/Maven-repository/tree/master/fr/splayce/)
- use [our public Maven repository](https://github.com/Imaginatio/Maven-repository/)
- check out the [API reference](http://imaginatio.github.io/REL/api/)


## License

Copyright &copy; 2012 Imaginatio SAS

REL is released under the [MIT License](http://www.opensource.org/licenses/MIT)


## Authors

REL was developed at [Imaginatio](http://imaginatio.fr) for project [Splayce](http://splayce.com) by:

- [Adrien Lavoillotte](http://instanceof.me/) ([@streetpc](https://github.com/streetpc))
- Julien Martin

Contributors:

- Guillaume Vauvert ([@gvauvert](https://github.com/gvauvert)) designed the `TrackString` algorithm
