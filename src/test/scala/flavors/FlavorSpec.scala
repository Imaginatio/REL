package fr.splayce.REL.flavors

import org.specs2.mutable._

import fr.splayce.REL._


class FlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "Default translation" should {

    val tr = { (re: RE) => FlavorSpec.express(re)._1 }
    val a = RE("a")
    val c = RE("c")

    "translate recursively" in {
      tr(a)                   must_== "b"
      tr(a %)                 must_== "(?:b)"
      tr(a?)                  must_== "(?:b)?"
      tr(a+)                  must_== "(?:b)+"
      tr(a*)                  must_== "(?:b)*"
      tr(a^2)                 must_== "(?:b){2}"
      tr(a(1 to 3))           must_== "(?:b){1,3}"
      tr(a(3))                must_== "(?:b){3,}"
      tr(RepAtMostN(a,3))     must_== "b{0,3}"
      tr(a | c)               must_== "b|d"
      tr(a - c)               must_== "bd"
      tr(a ~ c)               must_== "(?:b)(?:d)"
      tr(a \ "g")             must_== "(b)"
      tr(a.>?)                must_== "(?=b)"
      tr((c | (a(3)++)) - a)  must_== "d|(?:(?:b){3,})++b"
    }

  }

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
      tr(("b" | (μ(3)++)) - "a") must_== "b|(?>(?:(?:[a-zA-Z0-9_]){3,})+)a"
    }



    "not crash when translating Date regexes" in {
      /* println */ (tr(matchers.fr.Date.FULL))
      /* println */ (tr(matchers.en.Date.FULL))
      1 === 1
    }

  }
}

object FlavorSpec extends util.Flavor {

  override def translate(re: RE): RE = re match {
    case Atom(a) if (a.toString == "a") => Atom("b".r)
    case Atom(c) if (c.toString == "c") => Atom("d".r)
    case _                              => super.translate(re)
  }
}