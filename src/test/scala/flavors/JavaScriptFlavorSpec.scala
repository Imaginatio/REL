package fr.splayce.rel.flavors

import org.specs2.mutable._

import fr.splayce.rel._


class JavaScriptFlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE
  import scala.util.matching.Regex

  "JavaScript Flavor translation" should {

    val tr = { (re: RE) => JavaScriptFlavor.express(re)._1 }
    val notSupported = " not supported in JavaScript"
    val a = RE("a")

    "not support look-behind, keep look-ahead" in {
      val msg = "LookBehind is" + notSupported
      tr(?<=(a))  must throwA[IllegalArgumentException](message = msg)
      tr(?<!(a))  must throwA[IllegalArgumentException](message = msg)
      tr( ?=(a))  must_== "(?=a)"
      tr( ?!(a))  must_== "(?!a)"
    }

    "transform atomic grouping" in {
      val ga = a.g
      tr(?>(a)) must_== (?=(ga) - !ga).ncg.toString

      // checking the atomic nature
      new Regex(tr(?>("abc|ab")) + "c").findFirstMatchIn("abc") must beNone
    }

    "transform possessive quantifiers" in {
      tr(a?+)                           must_== "(?:(?=(a?))\\1)"
      tr(a++)                           must_== "(?:(?=(a+))\\1)"
      tr(a*+)                           must_== "(?:(?=(a*))\\1)"
      tr(RepAtLeastN(a, 2, Possessive)) must_== "(?:(?=(a{2,}))\\1)"
      tr(RepAtMostN (a, 2, Possessive)) must_== "(?:(?=(a{0,2}))\\1)"
      tr(RepNToM (a, 2, 5, Possessive)) must_== "(?:(?=(a{2,5}))\\1)"

      // checking the possessive nature
      new Regex(tr(a?+) + "a").findFirstMatchIn("a")                              must beNone
      new Regex(tr(a++) + "a").findFirstMatchIn("aa")                             must beNone
      new Regex(tr(a*+) + "a").findFirstMatchIn("a")                              must beNone
      new Regex(tr(RepAtLeastN(a, 2, Possessive)) + "a").findFirstMatchIn("aaaa") must beNone
      new Regex(tr(RepAtMostN (a, 2, Possessive)) + "a").findFirstMatchIn("aa")   must beNone
      new Regex(tr(RepNToM (a, 2, 5, Possessive)) + "a").findFirstMatchIn("aaaa") must beNone
    }

    "not support unicode categories" in {
      def msg(s: String) = "Unicode categories \\(including " + s + "\\) are" + notSupported

      tr(LetterLower) must throwA[IllegalArgumentException](message = msg("LetterLower"))
      tr(LetterUpper) must throwA[IllegalArgumentException](message = msg("LetterUpper"))
      tr(λ)           must throwA[IllegalArgumentException](message = msg("Letter"))
      tr(Λ)           must throwA[IllegalArgumentException](message = msg("NotLetter"))
    }

    "not support local mode modifiers" in {
      tr(a.ncg("i")) must throwA[IllegalArgumentException](message = "Local mode modifiers \\(NCG flags\\) are" + notSupported)
    }

    "translate \\A and \\z" in {
      tr(^^) must_== "^"
      tr($$) must_== "$"
    }
    "keep \\w, \\W, \\b and \\B" in {
      tr(μ) must_== "\\w"
      tr(Μ) must_== "\\W"
      tr(ß) must_== "\\b"
      tr(Β) must_== "\\B"
    }

    "not translate raw regexes" in {
      val re = "\\p{L}.(?<!\\w)"
      tr(re) must_== re
    }

    "not be able to translate Date regex" in {
      tr(matchers.en.Date.ALL) must throwA[IllegalArgumentException](message = "LookBehind is" + notSupported)
    }

    "translate recursively" in {
      tr(("b" | (^^{3}++)) - "a") must_== "b|(?:(?=((?:^{3})+))\\1)a"
    }

  }

}