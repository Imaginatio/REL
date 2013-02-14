package fr.splayce.rel.util

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.rel._


class ByOptionExtractor[+A](val regex: Regex, val extractMatch: Match => Option[A])
extends Extractor[A] {

  def this(re: RE, extract: MatchGroup => Option[A]) =
    this(re r, extract compose (re.matchGroup.apply _))

  def apply(in: String): Iterator[A] =
    (regex findAllIn in).matchData flatMap (extractMatch(_: Match).iterator)

  def apply(m: Match): Option[A] =
    extractMatch(m)

  /** For pattern matching on a Match (Extractors being
   *  already able to pattern matching on a String)
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
