package fr.splayce.REL.flavors

import org.specs2.mutable._

import fr.splayce.REL._
import util._


class FlavorSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "Default translation" should {

    val tr = { (re: RE) => FlavorSpec.simpleFlavor.express(re)._1 }
    val a = RE("a")
    val c = RE("c")

    "translate recursively" in {
      tr(a)                   must_== "b"
      tr(a %)                 must_== "(?:b)"
      tr(a?)                  must_== "(?:b)?"
      tr(a+)                  must_== "(?:b)+"
      tr(a*)                  must_== "(?:b)*"
      tr(a{2})                must_== "(?:b){2}"
      tr(a(1 -> 3))           must_== "(?:b){1,3}"
      tr(a > 3)               must_== "(?:b){3,}"
      tr(a < 3)               must_== "(?:b){0,3}"
      tr(a | c)               must_== "b|d"
      tr(a - c)               must_== "bd"
      tr(a ~ c)               must_== "(?:b)(?:d)"
      tr(a \ "g")             must_== "(b)"
      tr(a.?=)               must_== "(?=b)"
      tr((c | (a{3}++)) - a)  must_== "d|(?:(?:b){3})++b"
    }

  }

}

object FlavorSpec {

  val simpleFlavor = Flavor {
    case Atom(a) if (a.toString == "a") => Atom("b".r)
    case Atom(c) if (c.toString == "c") => Atom("d".r)  }
}