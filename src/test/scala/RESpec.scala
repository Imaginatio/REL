package fr.splayce.rel

import org.specs2.mutable._

import fr.splayce.rel._
import util.OpRewriter
import Implicits.string2RE


class RESpec extends Specification {

  val re = ("a" g) - "b" - ("c" g)
  val trans: RE => String = _ match {
    case r: Conc  => "+"
    case r: Alt   => "|"
    case r: Group => "$"
    case r: Opt   => "?"
    case r        => r.toString
  }
  val transOp: OpRewriter[List[String]] = { (l, re) =>
    (re, l ::: List(trans(re)))
  }
  val nil = List.empty[String]


  "Traversable projection" should {
    "Do prefix traversal" in {
      re.traverse(TraversalOrder.Prefixed   ).map(trans).mkString === "++$ab$c"
    }
    "Do infix-pre traversal" in {
      re.traverse(TraversalOrder.InfixedPre ).map(trans).mkString === "$a+b+$c"
    }
    "Do infix-post traversal" in {
      re.traverse(TraversalOrder.InfixedPost).map(trans).mkString === "a$+b+c$"
    }
    "Do postfix traversal" in {
      re.traverse(TraversalOrder.Postfixed  ).map(trans).mkString === "a$b+c$+"
    }
  }

  "RE tree transformation" should {
    "Do prefix traversal" in {
      (nil /: re)(transOp, TraversalOrder.Prefixed   )._2.mkString === "++$ab$c"
    }
    "Do infix-pre traversal" in {
      (nil /: re)(transOp, TraversalOrder.InfixedPre )._2.mkString === "$a+b+$c"
    }
    "Do infix-post traversal" in {
      (nil /: re)(transOp, TraversalOrder.InfixedPost)._2.mkString === "a$+b+c$"
    }
    "Do postfix traversal" in {
      (nil /: re)(transOp, TraversalOrder.Postfixed  )._2.mkString === "a$b+c$+"
    }

    "Stop prefixed & infixed recusion when class changes" in {
      val transOpRE2change: OpRewriter[List[String]] = { (l, re) =>
        re match {
          case Conc(lRe, rRe) => (Alt(lRe, rRe), l ::: List("|"))
          case _              => transOp(l, re)
        }
      }
      (nil /: re)(transOpRE2change, TraversalOrder.Prefixed   )._2.mkString === "|"
      (nil /: re)(transOpRE2change, TraversalOrder.InfixedPre )._2.mkString === "$a||"
      (nil /: re)(transOpRE2change, TraversalOrder.InfixedPost)._2.mkString === "a$||"
      (nil /: re)(transOpRE2change, TraversalOrder.Postfixed  )._2.mkString === "a$b|c$|"

      val transOpRE1change: OpRewriter[List[String]] = { (l, re) =>
        re match {
          case g: Group => (g.re.?, l ::: List("?"))
          case _        => transOp(l, re)
        }
      }
      (nil /: re)(transOpRE1change, TraversalOrder.Prefixed   )._2.mkString === "++?b?"
      (nil /: re)(transOpRE1change, TraversalOrder.InfixedPre )._2.mkString === "?+b+?"
      (nil /: re)(transOpRE1change, TraversalOrder.InfixedPost)._2.mkString === "a?+b+c?"
      (nil /: re)(transOpRE1change, TraversalOrder.Postfixed  )._2.mkString === "a?b+c?+"
    }
  }

}