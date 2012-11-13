package fr.splayce.REL

import scala.util.matching.Regex
import Regex.Match


package object util {

  type Rewriter = PartialFunction[RE, RE]

  type MatchExtractor[+A]            = PartialFunction[Match, A]
  type MatchGroupExtractor[+A]       = PartialFunction[MatchGroup, A]
  type MatchGroupOptionExtractor[+A] = MatchGroupExtractor[Option[A]]

  /** Reverse of PartialFunction's lift method
    *
    * Use with parcimony if f is costly, for it may be called twice.
    * For example in such case, prefer using [[ByOptionExtractor]]
    * over using unlift to make a MatchExtractor.
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

  class ByOptionExtractor[+A](val regex: Regex, val extractMatch: Match => Option[A])
  extends Extractor[A] {

    def this(re: RE, extract: MatchGroup => Option[A]) =
      this(re r, extract compose (re.matchGroup.apply _))

    def apply(in: String): Iterator[A] =
      (regex findAllIn in).matchData flatMap (extractMatch(_: Match).iterator)

    def apply(m: Match): Option[A] =
      extractMatch(m)

    /** For pattern matching on a Match
      * (Extractors being naturally able
      * to pattern matching on a String)
      */
    lazy val toPM = new PatternMatcher[Match, A](extractMatch)

  }
  object ByOptionExtractor {
    import Predef.{DummyImplicit => DI} // avoid "Same erasure" error

    def lift[A](extract: MatchGroupOptionExtractor[A])           : MatchGroup => Option[A] =
      extract orElse defaultMGOE[A]
    def lift[A](extract: MatchGroupExtractor[A])(implicit d: DI) : MatchGroup => Option[A] =
      extract lift

    def firstNES[A](extract: MatchGroup => Option[A]) =
      { (mg: MatchGroup) => mg.neSubmatches.headOption flatMap extract }

    /** For pattern matching on a MatchGroup */
    def toPM[A](extract: MatchGroup => Option[A])     : PatternMatcher[MatchGroup, A] =
      new PatternMatcher(extract)
    /** For pattern matching on a MatchGroup */
    def toPM[A](extract: MatchGroupOptionExtractor[A]): PatternMatcher[MatchGroup, A] =
      toPM(lift(extract))
  }


  /** Utility for instanciating pattern matchers on various objects.
    * See [[ByOptionExtractor#toPM]], [[ByOptionExtractor.toPM]].
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