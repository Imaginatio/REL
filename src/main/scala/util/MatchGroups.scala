package fr.splayce.rel.util

import scala.util.matching.Regex
import Regex.Match


/** A Match with its [accepted/filtered] capturing groups.
 *
 *  @note accept is 1-based, like `m.group(i)`
 */
class MatchGroups(m: Match, accept: (Int, String) => Boolean) {
  def this(m: Match)                                = this(m, { (i, n) => true })
  def this(m: Match, groupNames: String => Boolean) = this(m, { (i, n) => groupNames(n) })
  def this(m: Match, groupNums:  Int    => Boolean)(implicit d: DummyImplicit)
    /* d avoids erasure / double definition */      = this(m, { (i, n) => groupNums(i) })

  lazy val toList: List[(String, Option[String])] = {
    val seq = for {
      i <- 0 until m.groupCount;
      val j = i + 1
      val k = if (i < m.groupNames.size) m.groupNames(i) else ""
      if (accept(j, k))
      val v = Option(m.group(i + 1))
    } yield (k, v)
    seq toList
  }

  lazy val toMap: Map[String, Option[String]] =
    toList toMap
}


/** Pattern Matching & extraction utilities */
object MatchGroups {

  /** Extract a non-null, non-empty named group from a Match */
  def get(ma: Match, groupName: String): Option[String] =
    if (! ma.groupNames.contains(groupName)) None
    else Option(ma.group(groupName)).filterNot(_.isEmpty)

  /** Check if the named group is non-null and non-empty in the Match */
  def has(ma: Match, groupName: String): Boolean =
    get(ma, groupName).isDefined


  /** Allows direct pattern matching against a match's capturing groups.
   *
   *  List match against the Match's group(i) values, meaning
   *  groups may be null or empty. The following matches a Match
   *  whose 1st group matched "lit", 2nd empty, any 3rd, 4th null
   *  (i.e. wasn't matched, e.g. in an unmatched alternative),
   *  any 5th group (extracted), and 0 or more groups afterward:
   *  {{{
   *  case MatchGroups("lit", "", _, null, extr, _*) => extr
   *  }}}
   */
  def unapplySeq(m: Match): Option[List[String]] =
    if (m.groupCount == 0) None
    else Some((1 to m.groupCount).map { i => m.group(i) } toList)


  /** Allows pattern matching against a match's capturing groups,
   *  with a List of Options (None if null, Some(str) otherwise).
   *
   *  {{{
   *  case MatchGroups.NotNull(Some(any), None, Some(_), _*) => any
   *  }}}
   */
  object NotNull {
    def unapplySeq(m: Match): Option[List[Option[String]]] =
      MatchGroups.unapplySeq(m) map { l => l map { s => Option(s) } }

    /** Allows pattern matching with a Map of name -> option
     *  (None if group value is null, Some(str) otherwise).
     *
     *  In case of multiple groups with the same name,
     *  the last occurrence will prevail.
     *  {{{
     *  case MatchGroups.NotNull.NamedMap(m) if (m contains "a") => m("a")
     *  }}}
     */
    object NamedMap {
      def unapply(m: Match): Option[Map[String, Option[String]]] = {
        NamedPairs.unapplySeq(m).map(_.toMap)
      }
    }
    /** Allows pattern matching with a List of name -> option pairs
     *  (None if group value is null, Some(str) otherwise).
     *
     *  {{{
     *  case MatchGroups.NotNull.NamedPairs(("a", a), _*) => a
     *  }}}
     */
    object NamedPairs {
      def unapplySeq(m: Match): Option[List[(String, Option[String])]] = {
        Some(new MatchGroups(m).toList)
      }
    }

  }

  /** Allows pattern matching against a match's capturing groups,
   *  with a List of Options (None if null or empty, Some(str) otherwise).
   *
   *  {{{
   *  case MatchGroups.NotEmpty(Some(any), None, Some(_), _*) => any
   *  }}}
  */
  object NotEmpty {
    def unapplySeq(m: Match): Option[List[Option[String]]] =
      MatchGroups.unapplySeq(m) map { l => l map { s => Option(s).filterNot(_.isEmpty) } }


    /** Allows pattern matching with a Map of name -> option
     *  (None if group value is null or empty, Some(str) otherwise).
     *
     *  In case of multiple groups with the same name,
     *  the last occurrence will prevail.
     *  {{{
     *  case MatchGroups.NotEmpty.NamedMap(m) if (m contains "a") => m("a")
     *  }}}
     */
    object NamedMap {
      def unapply(m: Match): Option[Map[String, Option[String]]] = {
        NamedPairs.unapplySeq(m).map(_.toMap)
      }
    }
    /** Allows pattern matching with a List of name -> option pairs
     *  (None if group value is null or empty, Some(str) otherwise).
     *
     *  {{{
     *  case MatchGroups.NotEmpty.NamedPairs(("a", a), _*) => a
     *  }}}
     */
    object NamedPairs {
      def unapplySeq(m: Match): Option[List[(String, Option[String])]] = {
        Some(new MatchGroups(m).toList map { e => (e._1, e._2.filterNot(_.isEmpty)) })
      }
    }
  }

}
