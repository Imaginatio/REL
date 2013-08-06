# Extractors

Extractors are meant to help you extract information from text, using the regexes you made, especially by using the text matched in the capturing groups of you regex.


## Reminder on capturing groups

Capturing groups will yield `String` object that may be empty (if the group was matched to an empty part) or `null` (if the group wasn't matched). For instance, matching the string `"A"` against the regex `"(A)(B?)(C)?"` will yield values `"A", "", null`.

For example, say we want to extract information form the captured matches of the following regex:

```scala
val abc = ("." \ "a") - (".".? \ "b") - ("." \ "c").?
```

An empty string won't match and strings longer than 3 characters will match multiple times. Thus, for each match, the possible results are:

String  | `a` (#1) | `b` (#2) | `c` (#3)
--------|----------|----------|---------
`"A"`   | `"A"`    | `""`     | null
`"AB"`  | `"A"`    | `"B"`    | null
`"ABC"` | `"A"`    | `"B"`    | `"C"`

Also, Java < 7 does not support named capturing groups, and Scala only emulates them, mapping a list of names given at the compilation of the Regex against the indexes of the capturing groups. Thus, it is risky to have multiple instances of the same group name. In practice, using `myMatch.group("myGroup")` seems to always refer to the last occurrence of the `myGroup`.

On the other hand, the `Match` object carries the full list of group names (in its eponymous `groupNames` val), and REL uses it to compute the group tree. Thus, you _can_ reuse the same group name in a single expression.


## The Extractor trait

The `Extractor` trait is mainly a function that takes in a `String` and gives an `Iterator` of the parametrized type, with utility methods for composing and pattern matching.

This `Extractor` trait works with a sub-extractor, which can be of two types:

- A `PartialFunction[Regex.Match, A]`, which is pattern matching-friendly 
- A `PartialFunction[Regex.Match, Option[A]]`, which can allow a bit more flexibility and/or performance

`RE` expressions offer a utility `<<` method to which you can pass in sub-extractors of either types, getting an `Extractor[A]` that you can `apply` to `String`s to perform extraction.


## Basic extractors

Some trivial sub-extractors are provided for convenience:

- The simplest extractor is `MatchedExtractor`, which only yields every matches as `String`s.
- `NthGroupExtractor` yields the content matched by the nth capturing group, with `n` defaulting to `1`.
- `NamedGroupExtractor` does the same with the group holding the specified name.

Example:

```scala
val extractABC = abc << MatchedExtractor()
extractABC("1234567890").toList === List("123", "456", "789", "0")
val extractB = abc << NthGroupExtractor(2)
extractB("1234567890").toList === List("2", "5", "8", "")
val extractC = abc << NamedGroupExtractor("c")
extractC("1234567890").toList === List("3", "6", "9", null)
```


## MatchGroups for quick, flat Pattern Matching

If you want to do pattern matching on the list of strings matched by the capturing groups, you can use:

- `MatchGroups(val1, val2, …)` where `valn` are matched `String`s that may be `null` or empty.
- `NotNull(opt1, Some(val2), …)` where `optn` are `Option[String]`: `Some(valn)` if nth group matched (even if empty), `None` otherwise.
- `NotNull.NamedMap(map)` where `map` will be a `Map[String, Option[String]]` with group names as keys.
- `NotNull.NamedPairs(pair1, (name2, opt2), …)` where each pair is a `Tuple2[String, Option[String]]` with the group name and optional value.
- `NotEmpty`, `NotEmpty.NamedMap` and `NotEmpty.NamedPairs` if you don't care for empty matches: options will only be `Some(value)` if `value` is not an empty string.

Extractor examples:

```scala
import fr.splayce.rel.util.MatchGroups._
val pf: MatchExtractor[String] = {
  case NamedGroups("A", "", null)                 => "'A' only"
  case NotNull(Some("1"), Some(""), None)         => "'1' only"
  case NotNull.NamedMap(m) if (m contains "d")    => "unreachable"
  case NotNull.NamedPairs(_, ("b", Some("B")), _) => "b has 'B'"
  case NotEmpty(Some("x"), None, None)            => "'x' only"
  case NotEmpty.NamedMap(m) if (m contains "d")   => "unreachable"
  case NotEmpty.NamedPairs(_, ("b", Some(b)), _)  => "b has: " + b
}
val extract = re << pf
// extract(someString)
```


## Reusable extractors: MatchGroup hierarchies

One of the incentive for using REL is to reuse regex parts in other regexes. So we also need a way to reuse the corresponding extractors, including nesting them in other extractors.

Since a REL term is a tree, it can compute the resulting capturing groups tree with the `matchGroup` val, containing a tree of `MatchGroup`s. The top group corresponds to the entire match: it is unnamed, contains the matched content and has the first-level capturing groups nested as subgroups. When applied to a `Match`, it returns a copy of the capturing groups tree with the content filled for each group that matched. Thus, you can use pattern matching with nested groups to extract any group at several levels of imbrication with little code.

For example, let's say we want to match simple usernames that have the form `user@machine` where both part have only alphabetic characters. We can define the regex:

```scala
val user     = α.+ \ "user"
val at       = "@"
val machine  = α.+ \ "machine"
val username = (user - at - machine) \ "username"
```

And make a simple extractor that yields a tuple of Strings:

```scala
val userMatcher: PartialFunction[MatchGroup, (String, String)] = {
  case MatchGroup(None, Some(_), List(              // $0
      MatchGroup(Some("username"), Some(_), List(   // $1
        MatchGroup(Some("user"),    Some(u), Nil),  // $2
        MatchGroup(Some("machine"), Some(m), Nil)   // $3
      ))
    )) => (u, m)
}
```

Extraction in a String can be done like this:

```scala
import ByOptionExtractor._   // lift (and toPM later)
val userExtractor = username << lift(userMatcher)
val users = userExtractor("me@dev, you@dev")  // Iterator
users.toList.toString === "List((me,dev), (you,dev))"
```
BTW, you don't need `lift` if you use a `Function[MatchGroup, Option[A]` instead of a `PartialFunction[MatchGroup, A]`.

Since REL supports multiple capturing groups with the same name, we can extract items formatted with `username->username`:

```scala
val interaction = username - "->" - username
val iaMatcher: PartialFunction[MatchGroup, (String, String)] = {
  case MatchGroup(None, Some(_), List(
      MatchGroup(Some("username"), Some(un1), _),
      MatchGroup(Some("username"), Some(un2), _)
    )) => (un1, un2)
}
val iaExtractor = interaction << lift(iaMatcher)
val interactions =
  iaExtractor("me@dev->you@dev, you@dev->me@dev")
interactions.toList.toString ===
  "List((me@dev,you@dev), (you@dev,me@dev))"
```

And then you make a reusable extractor, **which can directly provide the extracted object**. Just place the extractor one level deeper to avoid the `$0` group:

```scala
val userMatcher2: PartialFunction[MatchGroup, (String, String)] = {
  case MatchGroup(Some("username"), Some(_), List(
      MatchGroup(Some("user"),    Some(u), Nil),
      MatchGroup(Some("machine"), Some(m), Nil)
    )) => (u, m)
}
val userPattern = toPM(lift(userMatcher2))
val iaMatcher2: PartialFunction[MatchGroup,
       (String, String, String, String)] = {
  case MatchGroup(None, Some(_), List(
      userPattern(u1, m1),
      userPattern(u2, m2)
    )) => (u1, m1, u2, m2)
}
val iaExtractor2 = interaction << lift(iaMatcher2)
val interactions2 =
  iaExtractor2("me@dev->you@dev, you@dev->me@dev")
interactions2.toList.toString ===
  "List((me,dev,you,dev), (you,dev,me,dev))"
```

In the same way, there are date extractor bundled in REL that can extract dates from strings, each match giving a list of possible dates interpretations (to account for ambiguity). See the doc on [Matchers](Matchers.md) for more details.

The following example demonstrates the use of **pattern matching directly on a `String`**:

```scala
val nfDateX = fr.splayce.rel.matchers.DateExtractor.NUMERIC_FULL
"From 21/10/2000 to 21/11/2000" match {
  case nfDateX(List(a), List(b)) => (a.m, b.m) === (10, 11)
}
```


## Debugging

Finally, the `toString` representation of a `MatchGroup` can be really helpful when debugging an Extractor or a regex.

    scala> val nfd = fr.splayce.rel.matchers.Date.NUMERIC_FULL
    scala> nfd.matchGroup
    res1: fr.splayce.rel.util.MatchGroup = 
    None	None
    	Some(n_f)	None
    		Some(n_ymd)	None
    			Some(n_sep)	None
    			Some(n_sep)	None
    		Some(n_dmy)	None
    			Some(n_sep)	None
    			Some(n_sep)	None

The top group has no name (first column is `None`), for it represents the whole match. We can see the sub-hierarchy of named groups, but it has no content yet. To fill the content, it must be applied to a `Match`:

    nfd.matchGroup(nfd.r.findFirstMatchIn("1998-10-20").get)
    res2: fr.splayce.rel.util.MatchGroup = 
    None	Some(1998-10-20)
    	Some(n_f)	Some(1998-10-20)
    		Some(n_ymd)	Some(1998-10-20)
    			Some(n_sep)	Some(-)
    			Some(n_sep)	None
    		Some(n_dmy)	None
    			Some(n_sep)	None
    			Some(n_sep)	None

Then we can see which groups matched which part.