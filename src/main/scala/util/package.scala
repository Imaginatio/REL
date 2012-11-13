package fr.splayce.REL

import scala.util.matching.Regex
import Regex.Match


package object util {

  type Rewriter = PartialFunction[RE, RE]

  type MatchExtractor[+A]       = PartialFunction[Match, A]
  type MatchOptionExtractor[+A] = Match => Option[A]

  /** Reverse of PartialFunction's lift method
    *
    * Use with parcimony if f is costly, for it may be called twice.
    * For example in such case, prefer implementing [MatchOptionExtractor]
    * over using unlift to make a MatchExtractor.
    */
  def unlift[A, B](f: Function1[A, Option[B]]): PartialFunction[A, B] = {
    case o if (f(o).isDefined) => f(o).get
  }

}

package util {

  trait Extractor[+A] extends Function1[String, Iterator[A]] {

    def compose(prepare: String => String) =
      Extractor(super.compose(prepare))

    def unapplySeq(in: String): Option[List[A]] =
      Some(apply(in) toList)

  }
  object Extractor {

    def apply[A](extractor: String => Iterator[A]) = new Extractor[A] {
      def apply(in: String) = extractor(in)
    }

  }

  trait ByOptionExtractor[+A] extends Extractor[A] {

    val regex: Regex

    def extractMatch(m: Match): Option[A]

    def apply(in: String): Iterator[A] =
      regex.findAllIn(in).matchData.flatMap(extractMatch(_))
    
    def apply(m: Match): Option[A] =
      extractMatch(m)

    lazy val matchPattern = MatchPattern(extractMatch _)

  }

  
  // Utility for pattern matching on Match objects

  trait MatchPattern[+A] {
    def unapply(m: Match): Option[A]
  }
  object MatchPattern {
    def apply[A](extractor: MatchOptionExtractor[A]) = new MatchPattern[A] {
      def unapply(m: Match) = extractor(m)
    }
    def apply[A](extractor: MatchExtractor[A]): MatchPattern[A] =
      apply(extractor lift)
  }


  // Trivial implementations

  case class MatchedExtractor() extends MatchExtractor[String] {
    def isDefinedAt(m: Match) = true
    def apply(m: Match) = m.matched
  }
  case class NthGroupExtractor(n: Int = 1) extends MatchExtractor[String] {
    def isDefinedAt(m: Match) = m.groupCount >= n
    def apply(m: Match) = m.group(n)
  }
  case class NamedGroupExtractor(name: String) extends MatchExtractor[String] {
    def isDefinedAt(m: Match) = m.groupNames contains name
    def apply(m: Match) = m.group(name)
  }

}