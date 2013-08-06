# Tree rewriting & Flavors

This chapter shows you how to recursively rewrite a REL expression, and how to use `Flavor`s to express your regex on other flavors/languages than Scala/Java.

## Subtree rewriting

An advantage to having a manipulable expression tree, other than reusing components, is that you can transform them as you please.

REL offers a way to do such manipulation quite simply using Scala's powerful pattern matching. By passing a `Rewriter` to a `RE` object's `map` method, you can recursively rewrite this object's subtree. A `Rewriter` is actually a `PartialFunction[RE, RE]`.

For example, we have a regex matching and capturing a [UUID](http://en.wikipedia.org/wiki/UUID) in its canonical form (lowercase hexadecimal, 8-4-4-4-12 digits). It is then used in a more complex expression as a capturing group. 

```scala
val s = RE("-")
val h = RE("[0-9a-f]")
val uuid = h{8} - s - h{4} - s - h{4} - s - h{4} - s - h{12}
val complexExpression = /* … */ a ~ (uuid \ "uuid1") ~
    b ~ (uuid \ "uuid2") ~ c /* … */
```

Say we want to match a `complexExpression` elsewhere, _without capturing the uuid_. We can just transform capturing our capturing `"uuid"` groups into non-capturing groups:

```scala
val toOther: Rewriter = {
  case Group(_, uuid, _) => uuid.ncg
}
val other = complexExpression map toOther
```

Now, say we want uppercase hexadecimal in this expression, `h` is being also used in other places than `uuid`. We can complete our `Rewriter`:

```scala
val H = RE("[0-9A-F]")
val toOther: Rewriter = {
  case `h` => H
  case Group(_, uuid, _) => uuid.ncg
}
val other = complexExpression map toOther
```

## Flavors

Other languages and tools have [other regex flavors](http://www.regular-expressions.info/refflavors.html), with (sometimes subtle) differences in implementation and additional or lacking features (with respect to Java's regex flavor). If we want to use our regexes in other flavors, we can apply some transformation to obtain compatible regexes (up to a point, the limit being unimplemented, unreplicable features).

- For some differences, a simple transformation will suffice. For instance, [.NET's regex flavor](http://www.regular-expressions.info/dotnet.html) considers that `\w` should match all letters, including diacritics (accented letters). Thus, `DotNETFlavor` will transform `\w`s (when used with `μ`/`Word`) into `[a-zA-Z0-9_]` to avoid unwanted surprises.
- For some lacking features, an exact equivalent exists. Possessive quantifiers are not implemented in .NET, but it supports atomic grouping, and a possessive quantifier is no more than an atomic grouping of a greedy quantifier. `DotNETFlavor` therefore changes `a++` into the equivalent expression `(?>a+)`.
- Other lacking feature can be emulated. [JavaScript's regex flavor](http://www.regular-expressions.info/javascript.html) does not support atomic grouping any more than possessive quantifiers. But atomic grouping may be emulated by capturing the expression in a look-ahead, then using an immediate back-reference to consume it without the possibility of backtracking. So `JavaScriptFlavor` mimics `a++` (or `(?>a+)`) with `(?=(a+))\1`. It is a stretch, since it add a possibly undesired capturing group, but it's still better than no support.
- Some lacking features unfortunately cannot be emulated. For instance, JavaScript does not support look-behind at all. There is no way to emulate this support, so `JavaScriptFlavor` will throw an `IllegalArgumentException` when you try to convert an expression containing a look-behind.
- Some additional features are implemented at REL level and can be used. Java priori to version 7 does not support inline naming of capturing groups, as .NET does. The `DotNETFlavor` (as well as the `Java7Flavor`) inlines the group names for capture (`(?<name>expr)`) and reference (`\k<name>`). 

`Flavor`s expose two main methods: `.express(re: RE)` and `.translate(re: RE)`. The first one returns a `Tuple2[String, List[String]]`, whose first element is the translated regex string and whose second is a list of the group names (in order of appearance) allowing you to perform a mapping to capturing group indexes (like Scala does) if needed. The second method only performs the translation of a `RE` term into another.

The following flavors are bundled with REL:

- [Java6](http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html), [Java7](http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html)
- [.NET](http://www.regular-expressions.info/dotnet.html)
- [JavaScript](http://www.regular-expressions.info/javascript.html)
- [PCRE](http://www.regular-expressions.info/pcre.html) (C, PHP, Ruby 1.9 / [Oniguruma](http://www.geocities.jp/kosako3/oniguruma/)…)
- [Legacy Ruby](http://www.regular-expressions.info/ruby.html) (Ruby 1.8, [does not support any Unicode](http://www.regular-expressions.info/unicode8bit.html))

For example, to express a regex in the .NET regex flavor:

```scala
val myRegex = ^^ - (α.++ \ "firstWord")
DotNETFlavor.translate(myRegex) // approximately* ^^ - (?>(α.+) \ "firstWord")
DotNETFlavor.express(myRegex)._1 === "\A(?<firstWord>(?>[a-zA-Z]+))"
DotNETFlavor.express(myRegex)._2.toString === "List(firstWord)"
```

\* approximately because the named capturing group will also have an inline naming strategy (for which there is no short DSL syntax, thus skipped here for the sake of simplicity)

But Flavors are not limited to other regex implementations. You can define your own for various uses, e.g.:

- maintaining an easily readable/maintainable tree in your code, injecting more capturing before runtime
- debugging existing regexes without altering the original `RE` tree
- extending pre-existing/vendor regexes
- reusing the same base regex in multiple contexts requiring small changes