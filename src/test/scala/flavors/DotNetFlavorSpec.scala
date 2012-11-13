package fr.splayce.REL.flavors

import org.specs2.mutable._

import fr.splayce.REL._


class DotNETFlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  ".NET Flavor translation" should {

    val tr = { (re: RE) => DotNETFlavor.express(re)._1 }

    "translate possessive quantifiers" in {
      tr("a" ?+)    must_== "(?>(?:a)?)"
      tr(RE("a")++) must_== "(?>(?:a)+)"
      tr("a" *+)    must_== "(?>(?:a)*)"
      tr(RepAtLeastN("a", 2, Possessive)) must_== "(?>a{2,})"
      tr(RepAtMostN ("a", 2, Possessive)) must_== "(?>a{0,2})"
      tr(RepNToM ("a", 2, 5, Possessive)) must_== "(?>a{2,5})"
    }

    "translate \\w and \\W" in {
      tr(μ) must_== "[a-zA-Z0-9_]"
      tr(Μ) must_== "[^a-zA-Z0-9_]"
    }

    "translate and inline named groups and references" in {
      val g = "a" \ "g"
      tr(g)      must_== "(?<g>a)"
      tr(g - !g) must_== "(?<g>a)\\k<g>"

      DotNETFlavor.express(g - g)._2 must_== List("g", "g")
    }

    "translate recursively" in {
      tr(("b" | (μ(3)++)) - "a") must_== "b|(?>(?:(?:[a-zA-Z0-9_]){3})+)a"
    }

    "not crash when translating Date regexes" in {
      /* println */ (tr(matchers.fr.Date.ALL))
      /* println */ (tr(matchers.en.Date.ALL))
      1 === 1
    }

  }

}