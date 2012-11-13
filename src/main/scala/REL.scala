package fr.splayce

import scala.collection.immutable.Map
import scala.util.matching.Regex
import Regex.Match


package REL {

  import util._

  abstract sealed class RepMode(val asString: String) {
    override def toString = asString
  }
  case object Greedy     extends RepMode("")
  case object Reluctant  extends RepMode("?")
  case object Possessive extends RepMode("+")

  abstract sealed class LookDirection(val asString: String) {
    override def toString = asString
  }
  case object Ahead  extends LookDirection("")
  case object Behind extends LookDirection("<")

  abstract sealed class RE {

    def ~ (that: RE)    =
      (this, that) match {
        case (Epsilon, Epsilon)                => Epsilon
        case (Epsilon, r)                      => r
        case (l, Epsilon)                      => l
        case (l: RECst, r @ Alt(_, _))         => l - r.ncg
        case (l, r)                            => l.ncg - r.ncg
      }

    def - (that: RE)    =
      (this, that) match {
        case (Epsilon, Epsilon) => Epsilon
        case (Epsilon, r)       => r
        case (l, Epsilon)       => l
        case (l, r)             => Conc(l, r)
      }

    def | (that: RE)    = Alt(this, that)

    def ?    = Opt(this.ncg)
    def ??   = Opt(this.ncg, Reluctant)
    def ?+   = Opt(this.ncg, Possessive)

    def *    = KStar(this.ncg)
    def *?   = KStar(this.ncg, Reluctant)
    def *+   = KStar(this.ncg, Possessive)

    def +    = KCross(this.ncg)
    def +?   = KCross(this.ncg, Reluctant)
    def ++   = KCross(this.ncg, Possessive)

    def apply(lb: Int, ub: Int, mode: RepMode = Greedy) = RepNToM(this.ncg, lb, ub, mode)
    def apply(rg: Range): RE      = apply(rg.start, rg.start + rg.length - 1)
    def apply(rg: (Int, Int)): RE = apply(rg._1, rg._2)

    def apply(n: Int): RE = RepExactlyN(this.ncg, n)
    def `>`  (n: Int): RE = RepAtLeastN(this.ncg, n)
    def `>?` (n: Int): RE = RepAtLeastN(this.ncg, n, Reluctant)
    def `>+` (n: Int): RE = RepAtLeastN(this.ncg, n, Possessive)
    def `<`  (n: Int): RE = RepAtMostN (this.ncg, n)
    // dotted form expr.<?(n) is mandatory,
    // standalone <? being syntactically significant in Scala (XMLSTART)
    def `<?` (n: Int): RE = RepAtMostN (this.ncg, n, Reluctant)
    def `<+` (n: Int): RE = RepAtMostN (this.ncg, n, Possessive)

    def `?=` : RE = LookAround(this, Ahead)
    def `?!` : RE = LookAround(this, Ahead, false)
    def `?<=`: RE = LookAround(this, Behind)
    def `?<!`: RE = LookAround(this, Behind, false)

    def g(name: String): Group = this match {
      case NCGroup(re) => re.g(name)
      case _ => Group(name, this)
    }
    def g(): Group = g("g" + java.util.UUID.randomUUID.toString.substring(24))
    def `\\` (name: String) = g(name)
    def apply(name: String) = g(name)
    def apply() = g()

    def ncg: RE = this match {
      case re: Wrapped => this
      case _           => NCGroup(this)
    }
    def % = ncg

    def ag: RE = this match {
      case re: Rep if (re.mode == Possessive) => this
      case re: AGroup                         => this
      case NCGroup(re)                        => re.ag
      case _                                  => AGroup(this)
    }
    def ?> = ag

    protected[REL] def linear(groupNames: List[String] = Nil): (String, List[String])
    lazy val lin = linear()

    lazy val r: Regex = new Regex(lin._1, lin._2.toArray: _*)

    override def toString = lin._1

    def map(tr: Rewriter): RE =
      (tr lift)(this) getOrElse recurseMap(tr)
    protected def recurseMap(tr: Rewriter): RE

    lazy val matchGroup: MatchGroup = MatchGroup(None, None, groups)
    val groups: List[MatchGroup]

    def <<[A](extractor: Match => Option[A]): Extractor[A] =
      new ByOptionExtractor[A](r, extractor)
    def <<[A](extractor: MatchGroup => Option[A])(implicit d: DummyImplicit): Extractor[A] =
      new ByOptionExtractor[A](this, extractor)
    def <<[A](extractor: MatchExtractor[A]): Extractor[A] =
      this << extractor.lift
  }

  /** A Wrapped RE needs no NCGroup protection */
  sealed trait Wrapped extends RE

  abstract sealed class RE2(val lRe: RE, val rRe: RE) extends RE {
    protected def linear(fn: (String, String) => String,
        groupNames: List[String]) = {
      val linL = lRe.linear(groupNames)
      val linR = rRe.linear(linL._2)
      (fn(linL._1, linR._1), linR._2)
    }

    override lazy val groups =
      lRe.groups ::: rRe.groups
  }

  case class Conc(override val lRe: RE,
      override val rRe: RE) extends RE2(lRe, rRe) {
    override def linear(groupNames: List[String]) =
      linear(_ + _, groupNames)

    override def recurseMap(tr: Rewriter) =
      Conc(lRe map tr, rRe map tr)
  }

  case class Alt(override val lRe: RE,
      override val rRe: RE) extends RE2(lRe, rRe) {
    override def linear(groupNames: List[String]) =
      linear(_ + "|" + _, groupNames)

    override def recurseMap(tr: Rewriter) =
      Alt(lRe map tr, rRe map tr)
  }


  sealed abstract class RE1(val re: RE) extends RE {
    protected def linear(fn: String => String,
        groupNames: List[String]) = {
      val lin = re.linear(groupNames)
      (fn(lin._1), lin._2)
    }

    override lazy val groups =
      re.groups
  }

  case class NCGroup(override val re: RE) extends RE1(re) with Wrapped {
    override def linear(groupNames: List[String]) = re match {
      case re: Wrapped => re.linear(groupNames)
      case _           => linear("(?:" + _ + ")", groupNames)
    }

    override def recurseMap(tr: Rewriter) =
      NCGroup(re map tr)
  }

  case class AGroup(override val re: RE) extends RE1(re) with Wrapped {
    override def linear(groupNames: List[String]) = re match {
      case re: Rep if (re.mode == Possessive) => re.linear(groupNames)
      case re: AGroup                         => re.linear(groupNames)
      case NCGroup(re)                        => AGroup(re).linear(groupNames)
      case _                                  => linear("(?>" + _ + ")", groupNames)
    }

    override def recurseMap(tr: Rewriter) =
      AGroup(re map tr)
  }

  case class Group(val name: String,
      override val re: RE) extends RE1(re) with Wrapped {
    override def linear(groupNames: List[String]) = re match {
      case NCGroup(re) => Group(name, re).linear(groupNames)
      case _           => linear("(" + _ + ")", groupNames ::: List(name))
    }

    override def recurseMap(tr: Rewriter) =
      Group(name, re map tr)

    override lazy val groups =
      List(MatchGroup(Some(name), None, re.groups))

    def unary_! = GroupRef(name)
  }

  case class LookAround(override val re: RE,
        val direction: LookDirection, positive: Boolean = true)
      extends RE1(re) with Wrapped {
    override def linear(groupNames: List[String]) = re match {
      case NCGroup(re) => LookAround(re, direction, positive).linear(groupNames)
      case _ => linear("(?" + direction + (if (positive) "=" else "!") + _ + ")", groupNames)
    }

    override def recurseMap(tr: Rewriter) =
      LookAround(re map tr, direction, positive)
  }

  sealed abstract class Rep(override val re: RE,
      val lb: Int,
      val ub: Option[Int] = None,
      val mode: RepMode = Greedy) extends RE1(re) {
    override def linear(groupNames: List[String]) =
      linear(_ + "{" + lb + "," + ub.getOrElse("") + "}" + mode, groupNames)
  }

  case class RepExactlyN(override val re: RE,
      val n: Int) extends Rep(re, n, Some(n), Greedy) {
    override def linear(groupNames: List[String]) =
      linear(_ + "{" + lb + "}" + mode, groupNames)

    override def recurseMap(tr: Rewriter) =
      RepExactlyN(re map tr, n)
  }

  case class RepNToM(override val re: RE,
        override val lb: Int,
        val max: Int,
        override val mode: RepMode = Greedy)
      extends Rep(re, lb, Some(max), mode) {
    override def recurseMap(tr: Rewriter) =
      RepNToM(re map tr, lb, max, mode)
  }

  case class RepAtLeastN(override val re: RE,
        override val lb: Int,
        override val mode: RepMode = Greedy)
      extends Rep(re, lb, None, mode) {
    override def recurseMap(tr: Rewriter) =
      RepAtLeastN(re map tr, lb, mode)
  }

  case class RepAtMostN(override val re: RE,
        val max: Int,
        override val mode: RepMode = Greedy)
         extends Rep(re, 0, Some(max), mode) {
    override def recurseMap(tr: Rewriter) =
      RepAtMostN(re map tr, max, mode)
  }

  case class Opt(override val re: RE,
        override val mode: RepMode = Greedy)
      extends Rep(re, 0, Some(1), mode) {

    override def linear(groupNames: List[String]) =
      linear(_ + "?" + mode, groupNames)

    override def recurseMap(tr: Rewriter) =
      Opt(re map tr, mode)
  }

  case class KStar(override val re: RE,
        override val mode: RepMode = Greedy)
      extends Rep(re, 0, mode = mode) {

    override def linear(groupNames: List[String]) =
      linear(_ + "*" + mode, groupNames)

    override def recurseMap(tr: Rewriter) =
      KStar(re map tr, mode)
  }

  case class KCross(override val re: RE,
        override val mode: RepMode = Greedy)
      extends Rep(re, 1, mode = mode) {

    override def linear(groupNames: List[String]) =
      linear(_ + "+" + mode, groupNames)

    override def recurseMap(tr: Rewriter) =
      KCross(re map tr, mode)
  }

  // should only be used for Flavors / tree transformation
  case class Wrapper(override val re: RE,
        val prefix: String, val suffix: String,
        appendGroupNames: List[String] = Nil)
      extends RE1(re) {

    override def linear(groupNames: List[String]) =
      linear(prefix + _ + suffix, groupNames ::: appendGroupNames)

    override def recurseMap(tr: Rewriter) =
      Wrapper(re map tr, prefix, suffix, appendGroupNames)
  }


  sealed abstract class RE0 extends RE {

    protected[REL] def linear(groupNames: List[String]) =
      (toString, groupNames)

    override def recurseMap(tr: Rewriter) = this

    override lazy val groups = Nil
  }

  case class GroupRef(val name: String) extends RE0 with Wrapped {

    override def linear(groupNames: List[String]) =
      ("\\" + (groupNames.lastIndexOf(name) + 1), groupNames)

    def unary_! = this
  }

  case class Atom(val re: Regex) extends RE0 {
    override def linear(groupNames: List[String]) =
      (re.toString, groupNames)
  }

  case class Escaped(val value: String) extends RE0 {
    def this(s: Symbol) = this(s.toString.substring(1))

    lazy val reStr = RE.escapeRegex(value)
    override def toString = reStr
  }

  abstract class REStr(val reStr: String) extends RE0 {
    override def toString = reStr
  }

  abstract class RECst(val reCst: String) extends REStr(reCst) with Wrapped

  case class Digit(val i: Int)
  extends RECst(if (i < 10) i.toString else "(?:" + i.toString + ")")

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


  object RE {
    val escapeChars = "\\^$()[]{}?*+.|"
    val escapeMap   = escapeChars map { c => c -> List('\\', c) } toMap
    def escapeRegex(c: Char): List[Char] = escapeMap.getOrElse(c, c :: Nil)
    def escapeRegex(s: String): String   = s flatMap escapeRegex

    def escape(s: String) = new Escaped(s)
    def escape(r: Regex)  = new Escaped(r.toString)

    def apply(s: Symbol) = new Escaped(s)
    def apply(s: String) = new Atom(s.r)
    def apply(r: Regex)  = new Atom(r)
    def apply(i: Int)    = new Digit(i)
  }


  object Implicits {
    implicit def regex2RE(r: Regex): RE    = RE(r)
    implicit def string2RE(s: String): RE  = RE(s)
    implicit def symbol2RE(s: Symbol): RE  = RE(s)
    implicit def int2RE(i: Int): RE        = RE(i)
    implicit def RE2String(re: RE): String = re.toString
    implicit def RE2Regex(re: RE): Regex   = re.r
  }

}


package object REL {

  // prefixed notation: ?>(a) is a.?>
  def `?>` (re: RE) = re.?>
  def `?=` (re: RE) = re.?=
  def `?!` (re: RE) = re.?!
  def `?<=`(re: RE) = re.?<=
  def `?<!`(re: RE) = re.?<!

  def esc(str: String) = RE.escape(str)

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
