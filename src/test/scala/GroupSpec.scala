package fr.splayce.rel

import org.specs2.mutable._

class GroupSpec extends Specification {

  import Symbols._
  import scala.util.matching.Regex
  import Implicits.string2RE
  import Implicits.RE2Regex


  "Non-capturing groups" should {
    val group = "a" %

    "linearize with (?:pattern)" in {
      group.toString must_== "(?:a)"
    }
    "linearize only once when nested" in {
      group.ncg.toString must_== "(?:a)"
    }
    "not linearize when direct subpattern is already grouping" in {
      "a".g.ncg.toString  must not startWith("(?:")
      "a".?<=.ncg.toString must not startWith("(?:")
      "a".?<!.ncg.toString must not startWith("(?:")
      "a".?=.ncg.toString must not startWith("(?:")
      "a".?!.ncg.toString must not startWith("(?:")
      "a".ag.ncg.toString must not startWith("(?:")
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
      m.group("n/a") must throwA[NoSuchElementException]
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