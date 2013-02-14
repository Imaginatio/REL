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
      tr(g)      must_== "(?P<g>a)"
      tr(g - !g) must_== "(?P<g>a)(?P=g)"

      PCREFlavor.express(g - g)._2 must_== List("g", "g")
    }

  }

}