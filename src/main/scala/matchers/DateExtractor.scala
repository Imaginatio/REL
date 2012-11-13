package fr.splayce.REL.matchers

import _root_.fr.splayce.REL
import REL.util._
import REL.Implicits.RE2Regex

import scala.util.matching.Regex
import Regex.Match

/**
 * Extract a list of possible date interpretations, considering ambiguity
 *
 * Ambiguous cases are (in expr A/B/C):
 * - for Date.NUMERIC and Date.NUMERIC_US:
 *     - YMD_S | DMY_S when 10 <= A <= 31 and C >= 10 (and A != C)
 * - for Date.NUMERIC_US
 *     - MDY_S | YMD_S when 0 < C <= 12 && B <= 12 (and (A != C or B != C))
 *     - MDY_S | DMY_S when B <= 12 (and A != B)
 *     - MDY_L | DMY_L when B <= 12 (and A != B)
 */

class DateExtractor(val regex: Regex = Date.NUMERIC)
extends ByOptionExtractor[List[DateExtractor.Result]] {
  import MatchGroups._
  import DateExtractor._

  def extractMatch(ma: Match): Option[List[Result]] = {
    if (ma.groupCount == 0) {
      return None
    }

    val n_y = get(ma, "n_y")
    if (n_y.isDefined) {
      return Some(List(Result(Some(n_y.get.toInt))))
    }

    val n_f = get(ma, "n_f")
    if (n_f.isDefined)
      n_f.get.split(Date.DATE_SEP) match {
        case Array(y, m, d) if (has(ma, "n_ymd")) => result(y, m, d, 'YMD_DMY)
        case Array(d, m, y) if (has(ma, "n_dmy")) => result(y, m, d)
        case Array(m, d, y) if (has(ma, "n_mdy")) => result(y, m, d, 'MDY_YMD, 'MDY_DMY)
        case Array(y, m)    if (has(ma, "n_ym"))  => result(y, m, "")
        case Array(m, y)    if (has(ma, "n_my"))  => result(y, m, "")
        case _ => None
      }
    else extractAlpha(ma)
  }

  def extractAlpha(ma: Match): Option[List[Result]] = None
}


object DateExtractor {

  val DAYS_IN_MONTH = Array(0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def year(y: String): String = year(y.toInt)
  def year(y: Int)  = (if (y > 100) "Y%04d ".format(y) else "") + "Y%02d".format(y % 100)
  def month(m: Int) = "M%02d".format(m)
  def day(d: Int)   = "D%02d".format(d)

  def result(ye: String, mo: String, da: String, ambiguous: Symbol*): Option[List[Result]] = {
    val y = if (ye.isEmpty) None else Some(ye.toInt)
    val m = if (mo.isEmpty) None else Some(mo.toInt)
    val d = if (da.isEmpty || (m.isDefined && da.toInt > DAYS_IN_MONTH(m.get))) None else Some(da.toInt)
    var r = List(Result(y, m, d))

    if (d.isEmpty && ambiguous.length == 1 && ambiguous(0) == 'YMD_DMY && y.get <= DAYS_IN_MONTH(m.get)) {
      return Some(List(Result(Some(da.toInt), m, y)))  // Longer, unambiguous date
    }

    for (switchable <- ambiguous) {
      r = r ::: (switchable match {
        case 'YMD_DMY if (y.get >= 10 && y.get <= DAYS_IN_MONTH(m.get) &&
            d.get >= 10 && d.get <= DAYS_IN_MONTH(m.get) &&
            d != y) =>
          List(Result(d, m, y))
        case 'MDY_YMD if (y.get >= 10 && y.get <= 12 && d.get <= 12 &&
            (d != m || m != y)) =>
          List(Result(m, d, y))
        case 'MDY_DMY if (d.get <= 12 && d != m) =>
          List(Result(y, d, m))
        case _ => Nil
      })
    }

    if (r.isEmpty) None else Some(r)
  }

  def result(y: Option[String], m: Int, d: Option[String]) =
    Some(List(Result(y.map(_.toInt), Some(m), d.map(_.toInt))))

  case class Result(
    val y: Option[Int] = None,
    val m: Option[Int] = None,
    val d: Option[Int] = None
  ) {
    require (y.isDefined || m.isDefined)
    require (y.isEmpty   || (0 <= y.get))
    require (m.isEmpty   || (0 <  m.get && m.get <= 12))
    require (d.isEmpty   || (0 <  d.get && d.get <= 31))

    lazy val str = {
      y.map(i => year(i)) .toList :::
      m.map(i => month(i)).toList :::
      d.map(i => day(i))  .toList
    }.mkString(" ")

    override def toString = str
  }
}


class AlphaDateExtractor(dateMatcher: AlphaDate) extends DateExtractor(dateMatcher.FULL) {
  import MatchGroups.get
  import DateExtractor._

  lazy val ALPHA_MONTHS_RE = dateMatcher.ALPHA_MONTHS.map(_.r.pattern)
  val NON_DIGITS = """\D""".r

  def monthNum(alphaMonth: String) =
    ALPHA_MONTHS_RE.indexWhere(_.matcher(alphaMonth).matches) + 1

  def dayNum(alphaDay: String) = NON_DIGITS.replaceAllIn(alphaDay, "")

  override def extractAlpha(ma: Regex.Match): Option[List[Result]] = {
    val am = get(ma, "a_m")
    if (am.isDefined)
      result(
        get(ma, "a_y"),
        monthNum(am.get),
        get(ma, "a_d").map(dayNum _))
    else
      None
  }
}

package fr {
  object DateExtractor extends AlphaDateExtractor(Date)
}

package en {
  object DateExtractor extends AlphaDateExtractor(Date)
}
