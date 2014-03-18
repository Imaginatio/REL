package fr.splayce.rel.matchers

import _root_.fr.splayce.rel
import rel.util._
import rel.RE
import rel.Implicits.RE2Regex

import scala.util.matching.Regex
import Regex.Match

import ByOptionExtractor._
import DateExtractor._
import AlphaDateExtractor._


/** Extract a list of possible date interpretations, considering ambiguity
 *
 *  Ambiguous cases are (in expression `A/B/C`):
 *  - for `Date.NUMERIC` and `Date.NUMERIC_US`:
 *    - `YMD_S | DMY_S` when `10 <= A <= 31 and C >= 10 (and A != C)`
 *  - for `Date.NUMERIC_US`
 *    - `MDY_S | YMD_S` when `0 < C <= 12 && B <= 12 (and (A != C or B != C))`
 *    - `MDY_S | DMY_S` when `B <= 12 (and A != B)`
 *    - `MDY_L | DMY_L` when `B <= 12 (and A != B)`
 */

class DateExtractor(
  val re: RE = Date.NUMERIC_FULL,
  val extract: DateExtractor.MGOE = DateExtractor.Numeric)
extends ByOptionExtractor[List[DateExtractor.Result]](re, firstNES(lift(extract)))

object DateExtractor {

  lazy val NUMERIC         = new DateExtractor(Date.NUMERIC)
  lazy val NUMERIC_US      = new DateExtractor(Date.NUMERIC_US)
  lazy val NUMERIC_FULL    = new DateExtractor(Date.NUMERIC_FULL)
  lazy val NUMERIC_FULL_US = new DateExtractor(Date.NUMERIC_FULL_US)

  type MGOE = MatchGroupOptionExtractor[List[DateExtractor.Result]]

  val NoDate: MGOE = { case _ => None }
  val NumericSub: MGOE = {
    case  MatchGroup(Some("n_ymd"), Some(n_ymd), _, _, _) =>
      val Array(y, m, d) = n_ymd.split(Date.DATE_SEP)
      result(y, m, d, 'YMD_DMY)
    case  MatchGroup(Some("n_dmy"), Some(n_dmy), _, _, _) =>
      val Array(d, m, y) = n_dmy.split(Date.DATE_SEP)
      result(y, m, d)
    case  MatchGroup(Some("n_mdy"), Some(n_mdy), _, _, _) =>
      val Array(m, d, y) = n_mdy.split(Date.DATE_SEP)
      result(y, m, d, 'MDY_YMD, 'MDY_DMY)
    case  MatchGroup(Some("n_ym"), Some(n_ym), _, _, _) =>
      val Array(y, m) = n_ym.split(Date.DATE_SEP)
      result(y, m, "")
    case  MatchGroup(Some("n_my"), Some(n_my), _, _, _) =>
      val Array(m, y) = n_my.split(Date.DATE_SEP)
      result(y, m, "")
  }
  val Numeric: MGOE = {
    case nf @ MatchGroup(Some("n_f"), Some(_), _, _, _) =>
      NumericSub(nf.neSubmatches.head)
    case MatchGroup(Some("n_y"), Some(n_y), _, _, _)    =>
      Some(List(Result(Some(n_y.toInt))))
  }

  val DAYS_IN_MONTH = Array(0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def year(y: Int)  = (if (y > 100) "Y%04d ".format(y) else "") + "Y%02d".format(y % 100)
  def month(m: Int) = "M%02d".format(m)
  def day(d: Int)   = "D%02d".format(d)

  def yearInt(y: String): Int = (if (y(0) == '\'') y.substring(1) else y).toInt

  def result(ye: String, mo: String, da: String, ambiguous: Symbol*): Option[List[Result]] = {
    val y = if (ye.isEmpty) None else Some(yearInt(ye))
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
    Some(List(Result(y.map(yearInt _), Some(m), d.map(_.toInt))))

  case class Result(
    val y: Option[Int] = None,
    val m: Option[Int] = None,
    val d: Option[Int] = None
  ) {
    require (y.isDefined || m.isDefined, "A Date Result needs at least a year or a month")
    require (y.isEmpty   || (0 <= y.get), "Incorrect year: " + y.get)
    require (m.isEmpty   || (0 <  m.get && m.get <= 12), "Incorrect month: " + m.get)
    require (d.isEmpty   || (0 <  d.get && d.get <= 31), "Incorrect day: " + m.get)

    lazy val str = {
      y.map(i => year(i)) .toList :::
      m.map(i => month(i)).toList :::
      d.map(i => day(i))  .toList
    }.mkString(" ")

    override def toString = str
  }
}


abstract class AlphaDateExtractor(
  val alphaDate: AlphaDate,
  val alphaMGOE: MGOE,
  val fullOnly: Boolean = false)
extends DateExtractor(
  if (fullOnly) alphaDate.ALL_FULL else alphaDate.ALL,
  alphaNumeric(alphaMGOE)) {

  lazy val AlphaNumeric = extract

  lazy val ALPHA_MONTHS_RE = alphaDate.ALPHA_MONTHS.map(_.r.pattern)

  def monthNum(alphaMonth: String) =
    ALPHA_MONTHS_RE.indexWhere(_.matcher(alphaMonth).matches) + 1
}
object AlphaDateExtractor {

  val NON_DIGITS = """\D""".r

  def dayNum(alphaDay: String) = NON_DIGITS.replaceAllIn(alphaDay, "")

  protected def alphaNumeric(alphaSub: MGOE): MGOE = {
    {
      case mg @ MatchGroup(Some("date"), Some(_), _, _, _) =>
        (alphaSub orElse Numeric)(mg.neSubmatches.head)
    }
  }
}

package fr {
  object     DateExtractor extends AlphaDateExtractor(Date, AlphaSub)
  object FullDateExtractor extends AlphaDateExtractor(Date, AlphaSub, true)
}
package object fr {
  import DateExtractor._

  val AlphaSub: MGOE = {
    case  MatchGroup(Some("a_f"), Some(_), List(
            MatchGroup(Some("a_d"), od, _, _, _),
            MatchGroup(Some("a_m"), Some(m), _, _, _),
            MatchGroup(Some("a_y"), oy, _, _, _)
          ), _, _) =>
      result(
        oy,
        monthNum(m),
        od.map(dayNum _))
  }
}

package en {
  object     DateExtractor extends AlphaDateExtractor(Date, AlphaSub)
  object FullDateExtractor extends AlphaDateExtractor(Date, AlphaSub, true)
}
package object en {
  import DateExtractor._

  val AlphaSub: MGOE = {
    case  MatchGroup(Some("a_f"), Some(_), List(
            MatchGroup(Some("a_d"), od, _, _, _),
            MatchGroup(Some("a_m"), Some(m), _, _, _),
            _, _,
            MatchGroup(Some("a_y"), oy, _, _, _)
          ), _, _) =>
      result(
        oy,
        monthNum(m),
        od.map(dayNum _))
    case  MatchGroup(Some("a_f"), Some(_), List(
            _, _,
            MatchGroup(Some("a_m"), Some(m), _, _, _),
            MatchGroup(Some("a_d"), od, _, _, _),
            MatchGroup(Some("a_y"), oy, _, _, _)
          ), _, _) =>
      result(
        oy,
        monthNum(m),
        od.map(dayNum _))
  }
}
