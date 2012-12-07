package fr.splayce.rel.flavors

import org.specs2.mutable._

import fr.splayce.rel._


class LegacyRubyFlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "Legacy Ruby Flavor translation" should {

    val tr = { (re: RE) => LegacyRubyFlavor.express(re)._1 }
    val notSupported = " not supported in Legacy Ruby"

    "translate possessive quantifiers" in {
      tr("a" ?+)    must_== "(?>a?)"
      tr(RE("a")++) must_== "(?>a+)"
      tr("a" *+)    must_== "(?>a*)"
      tr(RepAtLeastN("a", 2, Possessive)) must_== "(?>a{2,})"
      tr(RepAtMostN ("a", 2, Possessive)) must_== "(?>a{0,2})"
      tr(RepNToM ("a", 2, 5, Possessive)) must_== "(?>a{2,5})"
    }

    "not support look-behind, keep look-ahead" in {
      val msg = "LookBehind is" + notSupported
      tr(?<=("a"))  must throwA[IllegalArgumentException](message = msg)
      tr(?<!("a"))  must throwA[IllegalArgumentException](message = msg)
      tr( ?=("a"))  must_== "(?=a)"
      tr( ?!("a"))  must_== "(?!a)"
    }

    "not support unicode categories" in {
      def msg(s: String) = "Unicode categories \\(including " + s + "\\) are" + notSupported

      tr(LetterLower) must throwA[IllegalArgumentException](message = msg("LetterLower"))
      tr(LetterUpper) must throwA[IllegalArgumentException](message = msg("LetterUpper"))
      tr(λ)           must throwA[IllegalArgumentException](message = msg("Letter"))
      tr(Λ)           must throwA[IllegalArgumentException](message = msg("NotLetter"))
    }

    "translate LineTerminator" in {
      tr(Τ) must not contain "\\u"
    }

    "not translate raw regexes" in {
      val re = "\\p{L}.(?<!\\w)"
      tr(re) must_== re
    }

    "translate recursively" in {
      tr(("b" | (Τ{3}+)) - "a") must_== """b|(?:(?:\r\n?|\n){3})+a"""
    }

  }

}