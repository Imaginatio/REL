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

  }
}