package fr.splayce.rel

import scala.util.matching.Regex
import Regex.Match


package object util {

  type Rewriter      = PartialFunction[RE, RE]
  type OpRewriter[A] = Function2[A, RE, (RE, A)]

  /** Utility no-op Rewriter, bypasses recusion */
  val IdRewriter: Rewriter = { case re => re }

  /** Utility no-op Rewriter, keeps recusion */
  val RecursiveIdRewriter = new Rewriter {
    override def isDefinedAt(re: RE) = false
    override def apply(re: RE) = re
  }

  type MatchExtractor[+A]            = PartialFunction[Match, A]
  type MatchGroupExtractor[+A]       = PartialFunction[MatchGroup, A]
  type MatchGroupOptionExtractor[+A] = MatchGroupExtractor[Option[A]]

  /** Reverse of PartialFunction's lift method
   *
   *  Use with parcimony if f is costly, for it may be called twice.
   *  For example in such case, prefer using [[fr.splayce.rel.util.ByOptionExtractor]]
   *  over using unlift to make a MatchExtractor.
   */
  def unlift[A, B](f: Function1[A, Option[B]]): PartialFunction[A, B] = {
    case o if (f(o).isDefined) => f(o).get
  }

  def defaultMGOE[A]: MatchGroupOptionExtractor[A] = { case _ => None }

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


  /** Utility for instanciating pattern matchers on various objects.
   *
   *  @see fr.splayce.rel.util.ByOptionExtractor$.toPM[A](MatchGroup => Option[A]):PatternMatcher[MatchGroup, A]
   *  @see fr.splayce.rel.util.ByOptionExtractor$.toPM[A](MatchGroupOptionExtractor[A]):PatternMatcher[MatchGroup, A]
   */
  class PatternMatcher[-A, +B](val extract: A => Option[B]) {
    def this(partialExtractor: PartialFunction[A, B]) = this(partialExtractor lift)
    def unapply(a: A) = extract(a)
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