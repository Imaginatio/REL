package fr.splayce

import scala.util.matching.Regex


package object rel {

  // prefixed notations: ?>(a) is a.?>
  def `?>` (re: RE) = re.?>

  def `?=` (re: RE) = re.?=
  def `?!` (re: RE) = re.?!
  def `?<=`(re: RE) = re.?<=
  def `?<!`(re: RE) = re.?<!

  def esc(str: String) = RE.escape(str)

}


package rel {

  /** Quantifier repeating mode */
  abstract sealed class RepMode(val asString: String) {
    override val toString = asString
  }
  /** Greedy quantifier repeating mode */
  case object Greedy     extends RepMode("")
  /** Reluctant quantifier repeating mode */
  case object Reluctant  extends RepMode("?")
  /** Possessive quantifier repeating mode */
  case object Possessive extends RepMode("+")

  /** LookAround directions */
  abstract sealed class LookDirection(val asString: String) {
    override val toString = asString
  }
  /** LookAround direction: Ahead */
  case object Ahead  extends LookDirection("")
  /** LookAround direction: Behind */
  case object Behind extends LookDirection("<")

  /** Strategy to represent named capturing groups and references inline */
  class GroupNamingStyle(val capture: String => String, val reference: String => String)
  /** Named capturing group style for Java 7, ․NET. **/
  case object ChevNamingStyle extends GroupNamingStyle("?<"  + _ + ">", "\\k<" + _ + ">")
  /** Named capturing group style for ․NET. **/
  case object AposNamingStyle extends GroupNamingStyle("?'"  + _ + "'", "\\k'" + _ + "'")
  /** Named capturing group style for PCRE, Python. **/
  case object    PNamingStyle extends GroupNamingStyle("?P<" + _ + ">", "(?P=" + _ + ")")

  case object Epsilon         extends RECst("")
  case object Dot             extends RECst(".")
  case object MLDot           extends RECst("""[\s\S]""")
  case object LineTerminator  extends RECst("""(?:\r\n?|[\n\u0085\u2028\u2029])""")
  case object AlphaLower      extends RECst("[a-z]")
  case object AlphaUpper      extends RECst("[A-Z]")
  case object Alpha           extends RECst("[a-zA-Z]")
  case object NotAlpha        extends RECst("[^a-zA-Z]")
  case object LetterLower     extends RECst("""\p{Ll}""")
  case object LetterUpper     extends RECst("""\p{Lu}""")
  case object Letter          extends RECst("""\p{L}""")
  case object NotLetter       extends RECst("""\P{L}""")
  case object Digit           extends RECst("""\d""")
  case object NotDigit        extends RECst("""\D""")
  case object WhiteSpace      extends RECst("""\s""")
  case object NotWhiteSpace   extends RECst("""\S""")
  case object Word            extends RECst("""\w""")
  case object NotWord         extends RECst("""\W""")
  case object WordBoundary    extends RECst("""\b""")
  case object NotWordBoundary extends RECst("""\B""")
  case object LineBegin       extends RECst("^")
  case object LineEnd         extends RECst("$")
  case object InputBegin      extends RECst("""\A""")
  case object InputEnd        extends RECst("""\z""")

  object Implicits {
    /** Regex to RE term conversion (as `Atom`) */
    implicit def regex2RE(r: Regex): RE    = RE(r)
    /** String to RE term conversion (as `Atom`) */
    implicit def string2RE(s: String): RE  = RE(s)
    /** A to RE term conversion (as `Escaped`) */
    implicit def symbol2RE(s: Symbol): RE  = RE(s)
    /** A to RE term conversion (as `DigitCst`) */
    implicit def int2RE(i: Int): RE        = RE(i)
    /** RE to String linearization */
    implicit def RE2String(re: RE): String = re.toString
    /** RE to Regex linearization */
    implicit def RE2Regex(re: RE): Regex   = re.r
  }

  /** Shorthand symbols for common regex constants */
  object Symbols {
    val ^  = LineBegin
    val $  = LineEnd
    val ^^ = InputBegin
    val $$ = InputEnd
    val ε  = Epsilon
    val τ  = Dot
    val ττ = MLDot
    val Τ  = LineTerminator
    val α  = Alpha
    val Α  = NotAlpha
    val λ  = Letter
    val Λ  = NotLetter
    val δ  = Digit
    val Δ  = NotDigit
    val σ  = WhiteSpace
    val Σ  = NotWhiteSpace
    val μ  = Word
    val Μ  = NotWord
    val ß  = WordBoundary
    val Β  = NotWordBoundary
  }

}
