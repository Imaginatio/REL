package fr.splayce.rel

import org.specs2.mutable._

import fr.splayce.rel._
import Implicits.string2RE


class UtilSpec extends Specification {

  "Traversable projection" should {
    val re = "a" - "b" - ("c" g)
    val trans: RE => String = _ match {
      case r: Conc  => "+"
      case r: Group => "$"
      case r        => r.toString
    }

    "Do prefix traversal" in {
      re.traverse(TraversalOrder.Prefixed   ).map(trans).mkString === "++ab$c"
    }
    "Do infix-pre traversal" in {
      re.traverse(TraversalOrder.InfixedPre ).map(trans).mkString === "a+b+$c"
    }
    "Do infix-post traversal" in {
      re.traverse(TraversalOrder.InfixedPost).map(trans).mkString === "a+b+c$"
    }
    "Do postfix traversal" in {
      re.traverse(TraversalOrder.Postfixed  ).map(trans).mkString === "ab+c$+"
    }
  }

}