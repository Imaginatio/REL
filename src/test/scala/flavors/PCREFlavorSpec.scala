package fr.splayce.rel.flavors

import org.specs2.mutable._

import fr.splayce.rel._


object PCREFlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "PCRE Flavor translation" should {

    val tr = { (re: RE) => PCREFlavor.express(re)._1 }

    "translate and inline named groups and references, using P-style" in {
      val g = "a" \ "g"
      tr(g)      must_== "(?<g>a)"
      tr(g - !g) must_== "(?<g>a)\\k<g>"

      PCREFlavor.express(g - g)._2 must_== List("g", "g")
    }

    "use short LineTerminator \\R" in {
      tr("a" - Τ - "b") must_== """a\Rb"""
    }

    "keep group names when translating Date regexes" in {
      tr(matchers.fr.Date.ALL) must contain("(?<a_f>")
      tr(matchers.en.Date.ALL) must contain("(?<a_f>")
    }

    "strip repeated group names" in {
      val g = "a" \ "g"
      tr(g - !g - g - !g) must_== "(?<g>a)\\k<g>(a)\\2"
    }

  }

}