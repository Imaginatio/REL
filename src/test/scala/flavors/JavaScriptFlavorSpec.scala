package fr.splayce.rel.flavors

import org.specs2.mutable._

import fr.splayce.rel._


class JavaScriptFlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "JavaScript Flavor translation" should {

    val tr = { (re: RE) => JavaScriptFlavor.express(re)._1 }
    val notSupported = " not supported in JavaScript"

    "not support possessive quantifiers" in {
      val msg = "Possessive quantifiers are" + notSupported
      tr("a" ?+)    must throwA[IllegalArgumentException](message = msg)
      tr(RE("a")++) must throwA[IllegalArgumentException](message = msg)
      tr("a" *+)    must throwA[IllegalArgumentException](message = msg)
      tr(RepAtLeastN("a", 2, Possessive)) must throwA[IllegalArgumentException](message = msg)
      tr(RepAtMostN ("a", 2, Possessive)) must throwA[IllegalArgumentException](message = msg)
      tr(RepNToM ("a", 2, 5, Possessive)) must throwA[IllegalArgumentException](message = msg)
    }

    "not support look-behind, keep look-ahead" in {
      val msg = "LookBehind is" + notSupported
      tr("a".?<=)   must throwA[IllegalArgumentException](message = msg)
      tr("a".?<!)   must throwA[IllegalArgumentException](message = msg)
      tr("a".?=)   must_== "(?=a)"
      tr("a".?!)   must_== "(?!a)"
    }

    "not support atomic grouping" in {
      val msg = "Atomic grouping is" + notSupported
      tr("a".?>) must throwA[IllegalArgumentException](message = msg)
    }

    "not support unicode categories" in {
      def msg(s: String) = "Unicode categories \\(including " + s + "\\) are" + notSupported

      tr(LetterLower) must throwA[IllegalArgumentException](message = msg("LetterLower"))
      tr(LetterUpper) must throwA[IllegalArgumentException](message = msg("LetterUpper"))
      tr(λ)           must throwA[IllegalArgumentException](message = msg("Letter"))
      tr(Λ)           must throwA[IllegalArgumentException](message = msg("NotLetter"))
    }

    "translate \\A and \\z" in {
      tr(^) must_== "^"
      tr($) must_== "$"
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

    "translate recursively" in {
      tr(("b" | (^^{3}+)) - "a") must_== "b|(?:^{3})+a"
    }

  }

}