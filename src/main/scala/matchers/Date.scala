package fr.splayce.REL.matchers

import _root_.fr.splayce.REL._
import Implicits._
import Symbols._

object Date {

  val DATE_SEP = "[ /._-]"
  val s  = "n_sep"
  lazy val S  = DATE_SEP \ s
  
  lazy val YYYY = ("1[7-9]" | "20") ~ (δ - δ)
  lazy val MM   = "0[1-9]" | "1[012]"
  lazy val DD   = "0[1-9]" | ("[12][0-9]" | "3[01]")
  
  lazy val YY = δ - δ
  lazy val M  = "[1-9]" | "1[012]"
  lazy val D  = "[1-9]" | ("[12][0-9]" | "3[01]")
  
  lazy val DATE_YMD_L = YYYY - S ~ MM - !S ~ DD
  lazy val DATE_DMY_L = DD   ~ S ~ MM - !S - YYYY
  lazy val DATE_MDY_L = MM   ~ S ~ DD - !S - YYYY
  
  lazy val DATE_YMD_S = YY - S ~ M - !S ~ D
  lazy val DATE_DMY_S = D  ~ S ~ M - !S - YY
  lazy val DATE_MDY_S = M  ~ S ~ D - !S - YY
  
  lazy val DATE_YMD = (DATE_YMD_L | DATE_YMD_S) \ "n_ymd"
  lazy val DATE_DMY = (DATE_DMY_L | DATE_DMY_S) \ "n_dmy"
  lazy val DATE_MDY = (DATE_MDY_L | DATE_MDY_S) \ "n_mdy"

  lazy val DATE_YM = ( YYYY - S ~ MM ) \ "n_ym"
  lazy val DATE_MY = ( MM ~ S - YYYY ) \ "n_my"
  
  lazy val DATE    = (           DATE_YMD | DATE_DMY | DATE_YM | DATE_MY) \ "n_f"
  lazy val DATE_US = (DATE_MDY | DATE_YMD | DATE_DMY | DATE_YM | DATE_MY) \ "n_f"
  
  // longest first
  // val DATE_L = DATE_YMD_L \ "ymd_long" | DATE_DMY_L \ "dmy_long"
  // val DATE_S = DATE_YMD_S \ "ymd_short" | DATE_DMY_S \ "dmy_short"
  // val DATE = (DATE_L \ "d_long" | DATE_S \ "d_short") \ "d"

  lazy val NUMERIC    = δ.<! - (DATE    | YYYY \ "n_y") ~ δ.>!
  lazy val NUMERIC_US = δ.<! - (DATE_US | YYYY \ "n_y") ~ δ.>!

  // Ambiguity is shown in DateExtractor

}

import Date.{D, DD, YY, YYYY, NUMERIC_US}

abstract class AlphaDate {
  val NUMERIC = Date.NUMERIC

  val S: RE = " "
  val BREAK: RE = """(?<=\b|\.)"""
  val YEAR: RE = S.? ~ ((YYYY | YY) \ "a_y") ~ δ.>!

  val ALPHA_MONTHS: Array[String]
  lazy val ALPHA_MONTH = ALPHA_MONTHS.mkString("|") \ "a_m"

  val ALPHA : RE
  lazy val FULL = (ALPHA \ "a_f" | NUMERIC) \ "date"
}


package fr {

  object Date extends AlphaDate {

    override val ALPHA_MONTHS = Array(
      """janv(?:\.|ier)?""",
      """f[ée]v(?:\.|rier|r\.?)?""",
      """mars""",
      """avr(?:\.|il)?""",
      """mai""",
      """juin""",
      """juil(?:\.|let|l\.?)?""",
      """ao[uû]t""",
      """sept(?:\.|embre)?""",
      """oct(?:\.|obre)?""",
      """nov(?:\.|embre)?""",
      """d[ée]c(?:\.|embre)?""")

    val DAY = ("""1er\b""" | DD | D) \ "a_d"
    override val ALPHA = ((δ.<! - DAY ~ S.?) | ß) ~ ALPHA_MONTH ~ (YEAR | BREAK)
  }

}

package en {

  // Not all endianness are correctly supported
  // https://en.wikipedia.org/wiki/Calendar_date

  object Date extends AlphaDate {

    override val NUMERIC = NUMERIC_US

    override val ALPHA_MONTHS = Array(
      """jan(?:\.|uary)?""",
      """feb(?:r?\.?|ruary)?""",
      """mar(?:\.|ch)?""",
      """apr(?:\.|il)?""",
      """may""",
      """jun(?:\.|e)?""",
      """jul(?:\.|y)?""",
      """aug(?:\.|ust)?""",
      """sep(?:t?\.?|tember)?""",
      """oct(?:\.|ober)?""",
      """nov(?:\.|ember)?""",
      """dec(?:\.|ember)?""")

    val DAY = ("(?:(?:[23]?1)st|(?:2?2)nd|(?:2?3)rd|(?:[12]?[4-9]|[123]0)th)\\b"
      | DD | D ) \ "a_d"
    val YS: RE = ","
    override val ALPHA = 
      (((δ.<! - DAY ~ S.?) ~ ALPHA_MONTH) | (ß ~ ALPHA_MONTH ~ (S.? ~ DAY - δ.>!).?)) ~
      ((YS.? ~ YEAR) | BREAK)
  }

}
