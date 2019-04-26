package fr.splayce.rel.util

import scala.collection.SeqView
import scala.util.matching.Regex
import Regex.Match

case class MatchGroup(
    name: Option[String] = None,
    matched: Option[String] = None,
    subgroups: List[MatchGroup] = Nil,
    start: Int,
    end: Int) {

  def unapplySeq(m: Match): Option[List[MatchGroup]] = {
    Some(apply(m).subgroups)
  }

  lazy val submatches: SeqView[MatchGroup, List[MatchGroup]] =
    if (!matched.isDefined) List.empty[MatchGroup].view
    else subgroups.view.filter(_.matched.isDefined)
  lazy val neSubmatches: SeqView[MatchGroup, List[MatchGroup]] =
    submatches.filterNot(_.matched.get.isEmpty)

  def apply(m: Match): MatchGroup =
    apply(m, 0)._1

  def apply(m: Match, i: Int): (MatchGroup, Int) = {
    val nn =
      if (i == 0 || i - 1 >= m.groupNames.length) None
      else Option(m.groupNames(i - 1))
    val nm = Option(m.group(i))
    val l = subgroups.length

    var (j, k) = (i + 1, 0)
    var nsg: List[MatchGroup] = Nil
    while (j <= m.groupCount && k < l) {
      val g = subgroups(k)
      val (ng, nj) = g(m, j)
      nsg = nsg :+ ng
      k += 1
      j = nj
    }

    (MatchGroup(nn, nm, nsg, m.start(i), m.end(i)), j)
  }

  def toString(l: Int): String = {
    ("\t"*l) + name + "\t" + matched + "\n" +
      subgroups.map(_.toString(l+1)).mkString("")
  }
  override lazy val toString: String = toString(0)

}
