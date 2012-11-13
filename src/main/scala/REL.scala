package fr.splayce

package REL {

  import scala.collection.immutable.Map
  import scala.util.matching.Regex
  import scala.util.matching.Regex.Match
  
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
        case (l: RECst, r: RECst)              => l - r
        case (l @ Conc(_, _: RECst), r: RECst) => l - r
        case (l: RECst, r @ Conc(_, _: RECst)) => l - r
        case (l @ Alt(_, _), r: RECst)         => l.ncg - r
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

    def apply(lb: Int, ub: Int) = RepNToM(this.ncg, lb, ub)
    def apply(bs: Range): RE    = apply(bs.start, bs.start + bs.length - 1)
    def apply(lb: Int): RE      = RepAtLeastN(this.ncg, lb)
    def ^ (n: Int): RE          = RepExactlyN(this.ncg, n)
    
    def `>?` = LookAround(this, Ahead)
    def `>!` = LookAround(this, Ahead, false)
    def `<?` = LookAround(this, Behind)
    def `<!` = LookAround(this, Behind, false)
    
    def g(name: String): Group = this match {
      case NCGroup(re) => re.g(name)
      case _ => Group(name, this)
    }
    def g(): Group = g("g" + java.util.UUID.randomUUID.toString.substring(24))
    def `\\` (name: String) = g(name)
    def apply(name: String) = g(name)
    def apply() = g()
    
    def ncg: RE = this match {
      case re: LookAround => this
      case re: AGroup     => this
      case re: NCGroup    => this
      case re: Group      => this
      case _              => NCGroup(this)
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
    
    lazy val r: Regex = {
      val lin = linear()
      new Regex(lin._1, lin._2.toArray: _*)
    }
    
    override def toString = this.r.toString
   
  }

  abstract sealed class RE2(val lRe: RE, val rRe: RE) extends RE {
    protected def linear(fn: (String, String) => String,
        groupNames: List[String]) = {
      val linL = lRe.linear(groupNames)
      val linR = rRe.linear(linL._2)
      (fn(linL._1, linR._1), linR._2)
    }
  }

  case class Conc(override val lRe: RE,
      override val rRe: RE) extends RE2(lRe, rRe) {
    override def linear(groupNames: List[String]) =
      linear(_ + _, groupNames)
  }
  
  case class Alt(override val lRe: RE,
      override val rRe: RE) extends RE2(lRe, rRe) {
    override def linear(groupNames: List[String]) =
      linear(_ + "|" + _, groupNames)
  }
  
  
  sealed abstract class RE1(val re: RE) extends RE {
    protected def linear(fn: String => String,
        groupNames: List[String]) = {
      val lin = re.linear(groupNames)
      (fn(lin._1), lin._2)
    }
  }

  case class NCGroup(override val re: RE) extends RE1(re) {
    override def linear(groupNames: List[String]) = re match {
      case re: LookAround => re.linear(groupNames)
      case re: AGroup     => re.linear(groupNames)
      case re: NCGroup    => re.linear(groupNames)
      case re: Group      => re.linear(groupNames)
      case _              => linear("(?:" + _ + ")", groupNames)
    }
  }

  case class AGroup(override val re: RE) extends RE1(re) {
    override def linear(groupNames: List[String]) = re match {
      case re: Rep if (re.mode == Possessive) => re.linear(groupNames)
      case re: AGroup                         => re.linear(groupNames)
      case NCGroup(re)                        => AGroup(re).linear(groupNames)
      case _                                  => linear("(?>" + _ + ")", groupNames)
    }
  }

  case class Group(val name: String,
      override val re: RE) extends RE1(re) {
    override def linear(groupNames: List[String]) = re match {
      case NCGroup(re) => Group(name, re).linear(groupNames)
      case _           => linear("(" + _ + ")", groupNames ::: List(name))
    }
    
    def unary_! = GroupRef(name)
  }

  case class LookAround(override val re: RE,
        val direction: LookDirection, positive: Boolean = true)
      extends RE1(re) {
    override def linear(groupNames: List[String]) = re match {
      case NCGroup(re) => LookAround(re, direction, positive).linear(groupNames)
      case _ => linear("(?" + direction + (if (positive) "=" else "!") + _ + ")", groupNames)
    }
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
  }

  case class RepNToM(override val re: RE,
      override val lb: Int,
      val max: Int,
      override val mode: RepMode = Greedy)
    extends Rep(re, lb, Some(max), mode)

  case class RepAtLeastN(override val re: RE,
      override val lb: Int,
      override val mode: RepMode = Greedy)
    extends Rep(re, lb, None, mode)

  case class RepAtMostN(override val re: RE,
      val max: Int,
      override val mode: RepMode = Greedy)
       extends Rep(re, 0, Some(max), mode)

  case class Opt(override val re: RE,
        override val mode: RepMode = Greedy)
      extends Rep(re, 0, Some(1), mode) {
    override def linear(groupNames: List[String]) =
      linear(_ + "?" + mode, groupNames)
  }

  case class KStar(override val re: RE,
        override val mode: RepMode = Greedy)
      extends Rep(re, 0, mode = mode) {
    override def linear(groupNames: List[String]) =
      linear(_ + "*" + mode, groupNames)
  }

  case class KCross(override val re: RE,
        override val mode: RepMode = Greedy)
      extends Rep(re, 1, mode = mode) {
    override def linear(groupNames: List[String]) =
      linear(_ + "+" + mode, groupNames)
  }

  // should only be used for Flavors / tree transformation
  protected[REL] case class Wrapper(override val re: RE,
        val prefix: String, val suffix: String,
        appendGroupNames: List[String] = Nil)
      extends RE1(re) {
    override def linear(groupNames: List[String]) =
      linear(prefix + _ + suffix, groupNames ::: appendGroupNames)
  }


  sealed abstract class RE0 extends RE {
    protected[REL] def linear(groupNames: List[String]) =
      (toString, groupNames)
  }

  case class GroupRef(val name: String) extends RE0 {
    override def linear(groupNames: List[String]) =
      ("\\" + (groupNames.lastIndexOf(name) + 1), groupNames)

    def unary_! = this
  }

  case class Atom(val re: Regex) extends RE0 {
    override def linear(groupNames: List[String]) =
      (re.toString, groupNames)
  }
  
  case class Literal(val value: String) extends RE0 {
    def this(s: Symbol) = this(s.toString.substring(1))

    lazy val reStr = RE.escape(value)
    override def toString = reStr
  }

  abstract class RECst(val reStr: String) extends RE0 {
    override def toString = reStr
  }
  
  case class Digit(val i: Int) extends RECst(i.toString)
  
  case object Epsilon         extends RECst("")
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
  case object LineBeginning   extends RECst("^")
  case object LineEnd         extends RECst("$")
  case object InputBeginning  extends RECst("""\A""")
  case object InputEnd        extends RECst("""\z""")


  object RE {
    val escapeChars = "\\^$()[]{}?*+.|"
    val escapeMap = escapeChars map { c => c -> List('\\', c) } toMap
    def escapeChar(c: Char) = escapeMap.getOrElse(c, c :: Nil)
    def escape(s: String) = s flatMap escapeChar

    def literal(s: String) = new Literal(s)

    def apply(s: Symbol) = new Literal(s)
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

  object Symbols {
    val ε = Epsilon
    val α = Alpha
    val Α = NotAlpha
    val λ = Letter
    val Λ = NotLetter
    val δ = Digit
    val Δ = NotDigit
    val σ = WhiteSpace
    val Σ = NotWhiteSpace
    val μ = Word
    val Μ = NotWord
    val ß = WordBoundary
    val Β = NotWordBoundary
  }

}
