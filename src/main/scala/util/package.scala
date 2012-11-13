package fr.splayce.REL

import scala.util.matching.Regex
import Regex.Match


package object util {

  type Rewriter = PartialFunction[RE, RE]

  type Extractor[+A]            = String => Iterator[A]
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

  trait ByOptionExtractor[+A] extends Extractor[A] {

    val regex: Regex

    def extractMatch(m: Match): Option[A]

    def apply(in: String) =
      regex.findAllIn(in).matchData.flatMap(extractMatch(_))
  }

  object MatchGroups {

    def get(ma: Match, groupName: String): Option[String] =
      Option(ma.group(groupName)).filterNot(_.isEmpty)

    def has(ma: Match, groupName: String): Boolean =
      get(ma, groupName).isDefined

    def unapply(m: Match): Option[Map[String, Option[String]]] = {
      Some(new MatchGroups(m).groups)
    }
    def unapplySeq(m: Match): Option[Seq[(String, Option[String])]] = {
      unapply(m).map(_.toSeq)
    }
  }

  class MatchGroups(m: Match, groupNames: String => Boolean) {
    def this(m: Match) = this(m, s => true)

    lazy val groups: Map[String, Option[String]] = {
      m.groupNames map { n => if (groupNames(n)) n -> Option(m.group(n)) else n -> None } toMap
    }
  }


  // trivial implementations

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