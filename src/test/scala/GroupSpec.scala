package fr.splayce.rel

import org.specs2.mutable._

class GroupSpec extends Specification {

  import Symbols._
  import scala.util.matching.Regex
  import Implicits.string2RE
  import Implicits.RE2Regex


  "Non-capturing groups without flags" should {
    val group = "ab" %

    "linearize without (?:) on non-breaking subexpression" in {
      """a"""      .%.toString must_== """a"""
      """\\"""     .%.toString must_== """\\"""
      """^"""      .%.toString must_== """^"""
      """\t"""     .%.toString must_== """\t"""
      """\w"""     .%.toString must_== """\w"""
      """\cC"""    .%.toString must_== """\cC"""
      """\u00FF""" .%.toString must_== """\u00FF"""
      """\x0F"""   .%.toString must_== """\x0F"""
      """\0123"""  .%.toString must_== """\0123"""
      """\p{Lu}""" .%.toString must_== """\p{Lu}"""
      """[a-z]"""  .%.toString must_== """[a-z]"""
      """[^a-z]""" .%.toString must_== """[^a-z]"""
    }
    "linearize with (?:pattern) otherwise" in {
      group.toString must_== "(?:ab)"
    }
    "linearize only once when nested" in {
      group.ncg.toString must_== "(?:ab)"
    }
    "not linearize when direct subpattern is already grouping" in {
      "ab".g  .ncg.toString must not startWith("(?:")
      "ab".?<=.ncg.toString must not startWith("(?:")
      "ab".?<!.ncg.toString must not startWith("(?:")
      "ab".?= .ncg.toString must not startWith("(?:")
      "ab".?! .ncg.toString must not startWith("(?:")
      "ab".ag .ncg.toString must not startWith("(?:")
    }
  }

  "Non-capturing groups with flags" should {
    "linearize with (?flags:pattern)" in {
      ("ab" ncg "i-d" toString) must_== "(?i-d:ab)"
      ("i-d" ?: "ab"  toString) must_== "(?i-d:ab)"
    }
    "match inline flags" in {
      "abc"     must     be matching(RE.matchFlags)
      "-def"    must     be matching(RE.matchFlags)
      "abc-def" must     be matching(RE.matchFlags)
      "a-d"     must     be matching(RE.matchFlags)
      "abc-"    must not be matching(RE.matchFlags)
      "-"       must not be matching(RE.matchFlags)

      val RE.matchFlags(on, off) = "abc-def"
      on  must_== "abc"
      off must_== "def"
    }
    "combine immediately-nested NCG flags, prioritizing innermost flags" in {
      "ab".ncg("i-d").ncg       .toString must_== "(?i-d:ab)" // outside NCG is useless
      "ab".ncg.ncg("i-d")       .toString must_== "(?i-d:ab)" //  inside NCG is useless
      "ab".ncg("-d").ncg("i")   .toString must_== "(?i-d:ab)" // options are combined
      "ab".ncg("i-d").ncg("d-i").toString must_== "(?i-d:ab)" // innermost flags are preserved in conflicts
      val res = "ab".ncg("abc-xyz").ncg("dx-wa").asInstanceOf[NCGroup]
      res.withFlags   .toSet must_== "abcd".toSet
      res.withoutFlags.toSet must_== "wxyz".toSet
    }
    "be effective in Java regexes" in {
      "ab" must     be matching(       "ab")
      "ab" must     be matching("i" ?: "ab")
      "AB" must not be matching(       "ab")
      "AB" must     be matching("i" ?: "ab")
      "AB" must     be matching("-i" ?: ("i" ?: "ab").ag)
    }
  }

  "Atomic groups" should {
    val group = "a" ?>

    "linearize with (?>pattern)" in {
      group.toString must_== "(?>a)"
    }
    "linearize only once when nested" in {
      group.ag.toString must_== "(?>a)"
    }
    "not linearize when direct subpattern is already atomic (i.e. possessive repeater)" in {
      "a".?+.ag.toString     must not startWith("(?>")
      RE("a").++.ag.toString must not startWith("(?>")
      "a".*+.ag.toString     must not startWith("(?>")
      // possessive only
      "a".?.ag.toString      must     startWith("(?>")
      RE("a").+.ag.toString  must     startWith("(?>")
      RE("a").*.ag.toString  must     startWith("(?>")
      "a".??.ag.toString     must     startWith("(?>")
      RE("a").+?.ag.toString must     startWith("(?>")
      RE("a").*?.ag.toString must     startWith("(?>")
    }
    "linearize skipping direct subpattern when non-capturing group" in {
      "a".ncg.ag.toString must_== ("(?>a)")
    }
    "be atomic" in {
      val regex   = "a" - ("bc" | "b").?> - "c"
      val naRegex = "a" - ("bc" | "b").%  - "c"
      regex.toString   must_== "a(?>bc|b)c"
      naRegex.toString must_== "a(?:bc|b)c"

      "abcc" must     be matching(regex)
      "abc"  must not be matching(regex)
      "abcc" must     be matching(naRegex)
      "abc"  must     be matching(naRegex)
    }
  }

  "Capturing groups" should {
    val group = "a" \ "g"

    "linearize with (pattern)" in {
      group.toString must_== "(a)"
    }
    "linearize skipping direct subpattern when non-capturing group" in {
      "a".ncg.g.toString must_== ("(a)")
    }
    "provide group name" in {
      val m = group.r.findFirstMatchIn("abc").get
      m.group("g")   must_== "a"
      m.group("n/a") must throwA[IllegalArgumentException]
    }
    "generate arbitrary group name when not specified" in {
      val m = "a".g.r.findFirstMatchIn("abc").get
      m.groupNames.length must_== 1
      m.groupNames(0)     must_!= ""
    }
  }

  "Positive LookAhead groups" should {
    "linearize with (?=pattern)" in {
      "a".?=.toString must_== "(?=a)"
    }
    "linearize skipping direct subpattern when non-capturing group" in {
      "a".ncg.?=.toString must_== ("(?=a)")
    }
  }
  "Negative LookAhead groups" should {
    "linearize with (?!pattern)" in {
      "a".?!.toString must_== "(?!a)"
    }
    "linearize skipping direct subpattern when non-capturing group" in {
      "a".ncg.?!.toString must_== ("(?!a)")
    }
  }
  "Positive LookBehind groups" should {
    "linearize with (?<=pattern)" in {
      "a".?<=.toString must_== "(?<=a)"
    }
    "linearize skipping direct subpattern when non-capturing group" in {
      "a".ncg.?<=.toString must_== ("(?<=a)")
    }
  }
  "Negative LookBehind groups" should {
    "linearize with (?<!pattern)" in {
      "a".?<!.toString must_== "(?<!a)"
    }
    "linearize skipping direct subpattern when non-capturing group" in {
      "a".ncg.?<!.toString must_== ("(?<!a)")
    }
  }

  "Prefixed notation" should {
    val a = RE("a")
    "?>(expr)  --> expr.?>"  in { ?>(a)  === a.?> }
    "?=(expr)  --> expr.?="  in { ?=(a)  === a.?= }
    "?!(expr)  --> expr.?!"  in { ?!(a)  === a.?! }
    "?<=(expr) --> expr.?<=" in { ?<=(a) === a.?<= }
    "?<!(expr) --> expr.?<!" in { ?<!(a) === a.?<! }
  }

}