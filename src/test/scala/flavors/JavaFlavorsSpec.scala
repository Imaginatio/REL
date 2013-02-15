package fr.splayce.rel.flavors

import org.specs2.mutable._

import fr.splayce.rel._


object JavaFlavorsSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "Java 7 Flavor translation" should {

    val tr = { (re: RE) => Java7Flavor.express(re)._1 }

    "translate and inline named groups and references" in {
      val g = "a" \ "g"
      tr(g)      must_== "(?<g>a)"
      tr(g - !g) must_== "(?<g>a)\\k<g>"

      Java7Flavor.express(g - g)._2 must_== List("g", "g")
    }

    "strip invalid group names when translating Date regexes" in {
      tr(matchers.fr.Date.ALL) must not contain("(?<a_f>")
      tr(matchers.fr.Date.ALL) must not contain("(?<a_y>")
      tr(matchers.en.Date.ALL) must not contain("(?<a_f>")
      tr(matchers.en.Date.ALL) must not contain("(?<a_y>")
    }

    "strip repeated group names" in {
      val g = "a" \ "g"
      tr(g - !g - g - !g) must_== "(?<g>a)\\k<g>(a)\\2"
    }

  }


  "Java 6 Flavor translation" should {

    val tr = { (re: RE) => Java6Flavor.express(re)._1 }

    "strip inline named groups and references" in {
      val g = Group("g", "a", Some(ChevNamingStyle))
      g.toString must_== "(?<g>a)"
      tr(g)      must_== "(a)"
      tr(g - !g) must_== "(a)\\1"

      Java6Flavor.express(g - g)._2 must_== List("g", "g")
    }

    "not crash when translating Date regexes" in {
      (tr(matchers.fr.Date.ALL))
      (tr(matchers.en.Date.ALL))
      1 === 1
    }
  }
}