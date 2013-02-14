package fr.splayce.rel

import scala.collection.immutable.Map
import scala.util.matching.Regex
import Regex.Match

import util._

/** A REL term in the expression tree */
abstract sealed class RE {

  /** Protected concatenation, both operands are wrapped in non-capturing groups if needed */
  def ~ (that: RE)    =
    (this, that) match {
      case (Epsilon, Epsilon)          => Epsilon
      case (Epsilon, r)                => r
      case (l, Epsilon)                => l
      case (l, r)                      => l.ncg - r.ncg
    }

  /** Unprotected concatenation */
  def - (that: RE)    =
    (this, that) match {
      case (Epsilon, Epsilon) => Epsilon
      case (Epsilon, r)       => r
      case (l, Epsilon)       => l
      case (l, r)             => Conc(l, r)
    }

  /** Alternative */
  def | (that: RE)    = Alt(this, that)

  /** Zero-or-one quantifier, greedy */
  lazy val ?  = Opt(this.ncg)
  /** Zero-or-one quantifier, reluctant */
  lazy val ?? = Opt(this.ncg, Reluctant)
  /** Zero-or-one quantifier, possessive */
  lazy val ?+ = Opt(this.ncg, Possessive)

  /** Zero-or-more quantifier, greedy */
  lazy val *  = KStar(this.ncg)
  /** Zero-or-more quantifier, reluctant */
  lazy val *? = KStar(this.ncg, Reluctant)
  /** Zero-or-more quantifier, possessive */
  lazy val *+ = KStar(this.ncg, Possessive)

  /** One-or-more quantifier, greedy */
  lazy val +  = KCross(this.ncg)
  /** One-or-more quantifier, reluctant */
  lazy val +? = KCross(this.ncg, Reluctant)
  /** One-or-more quantifier, possessive */
  lazy val ++ = KCross(this.ncg, Possessive)

  /** N-to-M quantifier */
  def apply(lb: Int, ub: Int, mode: RepMode = Greedy) = RepNToM(this.ncg, lb, ub, mode)
  /** N-to-M quantifier */
  def apply(rg: Range): RE      = apply(rg.start, rg.start + rg.length - 1)
  /** N-to-M quantifier */
  def apply(rg: (Int, Int)): RE = apply(rg._1, rg._2)

  /** Exactely-N quantifier */
  def apply(n: Int): RE = RepExactlyN(this.ncg, n)
  /** At-least-N quantifier, greedy */
  def `>`  (n: Int): RE = RepAtLeastN(this.ncg, n)
  /** At-least-N quantifier, reluctant */
  def `>?` (n: Int): RE = RepAtLeastN(this.ncg, n, Reluctant)
  /** At-least-N quantifier, possessive */
  def `>+` (n: Int): RE = RepAtLeastN(this.ncg, n, Possessive)
  /** At-most-N quantifier, greedy */
  def `<`  (n: Int): RE = RepAtMostN (this.ncg, n)
  /** At-most-N quantifier, reluctant
   *
   *  Dotted form `expr.<?(n)` is mandatory, standalone `<?` being
   *  syntactically significant in Scala (`XMLSTART`)
   */
  def `<?` (n: Int): RE = RepAtMostN (this.ncg, n, Reluctant)
  /** At-most-N quantifier, possessive */
  def `<+` (n: Int): RE = RepAtMostN (this.ncg, n, Possessive)


  /** Positive look-ahead */
  lazy val `?=` : RE = LookAround(this, Ahead)
  /** Negative look-ahead */
  lazy val `?!` : RE = LookAround(this, Ahead, false)
  /** Positive look-behind */
  lazy val `?<=`: RE = LookAround(this, Behind)
  /** Negative look-behind */
  lazy val `?<!`: RE = LookAround(this, Behind, false)

  /** Named capturing group */
  def g(name: String): Group = this match {
    case NCGroup(re, "") => re.g(name)
    case _               => Group(name, this)
  }
  /** Unnamed capturing group (actual unique name is generated) */
  def g(): Group = g("g" + java.util.UUID.randomUUID.toString.substring(24))
  /** Named capturing group */
  def `\\` (name: String) = g(name)
  /** Unnamed capturing group (actual unique name is generated) */
  def apply() = g()

  /** Non-capturing group */
  lazy val ncg: Wrapped = this match {
    case re: Wrapped => re
    case _           => NCGroup(this)
  }
  /** Non-capturing group */
  lazy val % = ncg
  /** Non-capturing group with flags (local mode modifiers) */
  def ncg(flags: String): Wrapped =
    if (flags.isEmpty) this.ncg
    else this match {
      case inner: NCGroup => // combine flags, innermost (here, inner's flags) wins
        val RE.matchFlags(withFlags, withoutFlags) = flags
        val withF    = inner.withFlags.toSet    ++ withFlags.toSet    -- inner.withoutFlags.toSet
        val withoutF = inner.withoutFlags.toSet ++ withoutFlags.toSet -- inner.withFlags.toSet
        val f = withF.mkString("") + "-" + withoutF.mkString("")
        NCGroup(inner.re, if (f endsWith "-") f.substring(0, f.length - 1) else f)
      case _ => NCGroup(this, flags)
    }
  /** Non-capturing group with flags (local mode modifiers), infix operator `"i" ?: re` */
  def `?:`(flags: String): Wrapped = ncg(flags)

  /** Atomic group */
  lazy val ag: RE = this match {
    case re: Rep if (re.mode == Possessive) => this
    case re: AGroup                         => this
    case NCGroup(re, "")                    => re.ag
    case _                                  => AGroup(this)
  }
  /** Atomic group */
  lazy val ?> = ag

  def linear(groupNames: List[String] = Nil): (String, List[String])
  /** Combined linearization: first term is String representation, second is the List of capturing group names */
  lazy val lin = linear()

  /** Scala Regex linearization */
  lazy val r: Regex = new Regex(lin._1, lin._2.toArray: _*)

  /** String linearization */
  override def toString = lin._1

  /** Recursively replace matching terms in the RE subtree */
  def map(tr: Rewriter, order: RE.TraversalOrder = TraversalOrder.Postfixed): RE = {
    val tf = tr lift
    val to: OpRewriter[Null] = { (n, r) => (tf(r).getOrElse(r), n) }
    (null /: this)(to, order)._1
  }

  /** Recursively apply an `(RE, A) => (RE, A)` operation, going left to right.
   *
   *  In `Prefixed` and `InfixedPre` traversals, automatic recursion
   *  will only occur on `RE2` and `RE1` if the result of the operation
   *  remains of the same class.
   */
  def /:[A](z: A)(op: OpRewriter[A], order: RE.TraversalOrder): (RE, A)

  /** Return a Traversable for this order */
  def traverse(order: RE.TraversalOrder = TraversalOrder.Postfixed): Traversable[RE] = new Traversable[RE] {
    override def foreach[U](f: RE => U) {
      RE.this.foreach(order)(f)
    }
  }
  protected[rel] def foreach[U](order: RE.TraversalOrder)(f: RE => U)

  /** Corresponding MatchGroup tree, with containing unnamed `\$0` MatchGroup */
  lazy val matchGroup: MatchGroup = MatchGroup(None, None, groups)
  /** Corresponding MatchGroup tree, without containing unnamed `\$0` MatchGroup */
  val groups: List[MatchGroup]

  /** Generate an Extractor for this extracting function */
  def <<[A](extractor: Match => Option[A]): Extractor[A] =
    new ByOptionExtractor[A](r, extractor)
  /** Generate an Extractor for this extracting function */
  def <<[A](extractor: MatchGroup => Option[A])(implicit d: DummyImplicit): Extractor[A] =
    new ByOptionExtractor[A](this, extractor)
  /** Generate an Extractor for this extracting MatchExtractor */
  def <<[A](extractor: MatchExtractor[A]): Extractor[A] =
    this << extractor.lift
}

object RE {
  type TraversalOrder = TraversalOrder.Value

  /** Regex for non-breaking entities = that does not need NCGroup protection.
   *
   *  This regex matches:
   *  - single characters: `a`, `\w`, `\cX`, `\u0f1f`, `\h1f`, `\0123`
   *  - character classes `\p{...}`, `[...]`
   */
  val nonBreakingEntity = """^(?:\\?.|\\c.|\\u[\da-fA-F]{4}|\\x[\da-fA-F]{2}|\\0[0-3]?[0-7]{1,2}|\\[pP]\{\w+\}|\[[^\]]*+\])$""".r
  /** Regex for mode modifiers (flags) */
  val matchFlags = """^([a-zA-Z]*+)(?>-(?!$))?+([a-zA-Z]*+)$""".r

  /** Validator for Java 7-style strict group naming validation: 1 alpha + 0..* alphanumeric chars */
  val strictGroupName =  """^[a-zA-Z][a-zA-Z0-9]*$""".r
  /** Regex for ․NET/PCRE-style group naming validation: 1 alpha or `_` + 0..* alphanumeric chars or `_`s */
  val snakeGroupName =   """^[a-zA-Z_]\w*$""".r
  /** Regex for Ruby 1.9-style group naming validation: 1 alpha or `_` + 0..* ASCII printable chars but `>` */
  val lenientGroupName = """^[a-zA-Z_][ -=?-~]*$""".r

  val escapeChars = "\\^$()[]{}?*+.|"
  val escapeMap   = escapeChars map { c => c -> List('\\', c) } toMap
  def escapeRegex(c: Char): List[Char] = escapeMap.getOrElse(c, c :: Nil)
  def escapeRegex(s: String): String   = s flatMap escapeRegex

  def escape(s: String) = new Escaped(s)
  def escape(r: Regex)  = new Escaped(r.toString)

  def apply(s: Symbol) = new Escaped(s)
  def apply(s: String) = new Atom(s.r)
  def apply(r: Regex)  = new Atom(r)
  def apply(i: Int)    = new DigitCst(i)
}


/** A Wrapped RE needs no NCGroup protection */
sealed trait Wrapped extends RE


/** Two-operands RE tree node */
abstract sealed class RE2(val lRe: RE, val rRe: RE) extends RE {
  protected def replicate(lRe: RE = lRe, rRe: RE = rRe): RE2

  protected def linear(fn: (String, String) => String,
      groupNames: List[String]) = {
    val linL = lRe.linear(groupNames)
    val linR = rRe.linear(linL._2)
    (fn(linL._1, linR._1), linR._2)
  }

  override lazy val groups =
    lRe.groups ::: rRe.groups

  override def /:[A](z: A)(op: OpRewriter[A], order: RE.TraversalOrder): (RE, A) = order match {
    case TraversalOrder.Prefixed  =>
      val t = op(z, this)
      if (t._1.getClass == getClass) {
        val l = (t._2 /: t._1.asInstanceOf[RE2].lRe)(op, order)
        val r = (l._2 /: t._1.asInstanceOf[RE2].rRe)(op, order)
        (t._1.asInstanceOf[RE2].replicate(l._1, r._1), r._2)
      } else t
    case TraversalOrder.InfixedPre | TraversalOrder.InfixedPost =>
      val l = (z /: lRe)(op, order)
      val that = this.replicate(lRe = l._1, rRe = rRe)
      val t = op(l._2, that)
      if (t._1.getClass == getClass) {
        val r = (t._2 /: rRe)(op, order)
        (that.replicate(rRe = r._1), r._2)
      } else t
    case TraversalOrder.Postfixed =>
      val l = (   z /: lRe)(op, order)
      val r = (l._2 /: rRe)(op, order)
      op(r._2, this.replicate(lRe = l._1, rRe = r._1))
  }

  override def foreach[U](order: RE.TraversalOrder)(f: RE => U) {
    order match {
      case TraversalOrder.Prefixed  =>
        f(this) ; lRe.foreach(order)(f) ; rRe.foreach(order)(f)
      case TraversalOrder.InfixedPre | TraversalOrder.InfixedPost =>
        lRe.foreach(order)(f) ; f(this) ; rRe.foreach(order)(f)
      case TraversalOrder.Postfixed =>
        lRe.foreach(order)(f) ; rRe.foreach(order)(f) ; f(this)
    }
  }

}

/** Concatenation RE tree node */
case class Conc(override val lRe: RE,
    override val rRe: RE) extends RE2(lRe, rRe) {
  override def replicate(lRe: RE, rRe: RE) = copy(lRe, rRe)

  override def linear(groupNames: List[String]) =
    linear(_ + _, groupNames)
}

/** Alternative RE tree node */
case class Alt(override val lRe: RE,
    override val rRe: RE) extends RE2(lRe, rRe) {
  override def replicate(lRe: RE, rRe: RE) = copy(lRe, rRe)

  override def linear(groupNames: List[String]) =
    linear(_ + "|" + _, groupNames)
}


/** One-operand RE tree node */
sealed abstract class RE1(val re: RE) extends RE {
  protected def replicate(re: RE = re): RE1

  protected def linear(fn: String => String,
      groupNames: List[String]) = {
    val lin = re.linear(groupNames)
    (fn(lin._1), lin._2)
  }

  override lazy val groups =
    re.groups

  override def /:[A](z: A)(op: OpRewriter[A], order: RE.TraversalOrder): (RE, A) = order match {
    case TraversalOrder.Prefixed  | TraversalOrder.InfixedPre  =>
      val t = op(z, this)
      if (t._1.getClass == getClass) {
        val r = (t._2 /: t._1.asInstanceOf[RE1].re)(op, order)
        (t._1.asInstanceOf[RE1].replicate(r._1), r._2)
      } else t
    case TraversalOrder.Postfixed | TraversalOrder.InfixedPost =>
      val r = (z /: re)(op, order)
      op(r._2, if (re == r._1) this else this.replicate(r._1))
  }

  override def foreach[U](order: RE.TraversalOrder)(f: RE => U) {
    order match {
      case TraversalOrder.Prefixed | TraversalOrder.InfixedPre =>
        f(this) ; re.foreach(order)(f)
      case TraversalOrder.Postfixed | TraversalOrder.InfixedPost =>
        re.foreach(order)(f) ; f(this)
    }
  }
}

/** Non-capturing group */
case class NCGroup(override val re: RE, val flags: String = "") extends RE1(re) with Wrapped {
  val RE.matchFlags(withFlags, withoutFlags) = flags

  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) = re match {
    case re: Wrapped if flags.isEmpty => re.linear(groupNames)
    case _                            => linear(
      { s =>
        if (flags.isEmpty && RE.nonBreakingEntity.pattern.matcher(s).matches) s
        else "(?" + flags + ":" + s + ")"
      }, groupNames)
  }
}

/** Atomic group */
case class AGroup(override val re: RE) extends RE1(re) with Wrapped {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) = re match {
    case re: Rep if (re.mode == Possessive) => re.linear(groupNames)
    case re: AGroup                         => re.linear(groupNames)
    case NCGroup(re, "")                    => AGroup(re).linear(groupNames)
    case _                                  => linear("(?>" + _ + ")", groupNames)
  }
}

/** Named capturing group */
case class Group(val name: String, override val re: RE, val embedStyle: Option[GroupNamingStyle] = None)
extends RE1(re) with Wrapped {
  override def replicate(re: RE) = copy(re = re)

  override def linear(groupNames: List[String]) = re match {
    case NCGroup(re, "") => Group(name, re).linear(groupNames)
    case _               =>
      linear("(" + embedStyle.map(_ capture name).getOrElse("") + _ + ")", groupNames ::: List(name))
  }

  override lazy val groups =
    List(MatchGroup(Some(name), None, re.groups))

  lazy val unary_! = GroupRef(name, embedStyle)
}

/** Look-around */
case class LookAround(override val re: RE, val direction: LookDirection, positive: Boolean = true)
extends RE1(re) with Wrapped {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) = re match {
    case NCGroup(re, "") => LookAround(re, direction, positive).linear(groupNames)
    case _               => linear("(?" + direction + (if (positive) "=" else "!") + _ + ")", groupNames)
  }
}

/** Quantifier (aka “repeater”) */
sealed abstract class Rep(
  override val re: RE,
  val lb: Int,
  val ub: Option[Int] = None,
  val mode: RepMode = Greedy)
extends RE1(re) {

  override def linear(groupNames: List[String]) =
    linear(_ + "{" + lb + "," + ub.getOrElse("") + "}" + mode, groupNames)
}

/** Exactly-N quantifier */
case class RepExactlyN(override val re: RE, val n: Int)
extends Rep(re, n, Some(n), Greedy) {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) =
    linear(_ + "{" + lb + "}" + mode, groupNames)
}

/** N-to-M quantifier */
case class RepNToM(
  override val re: RE,
  override val lb: Int,
  val max: Int,
  override val mode: RepMode = Greedy)
extends Rep(re, lb, Some(max), mode) {

  override def replicate(re: RE) = copy(re)
}

/** At-least-N quantifier */
case class RepAtLeastN(override val re: RE, override val lb: Int, override val mode: RepMode = Greedy)
extends Rep(re, lb, None, mode) {
  override def replicate(re: RE) = copy(re)
}

/** At-most-N quantifier */
case class RepAtMostN(override val re: RE, val max: Int, override val mode: RepMode = Greedy)
extends Rep(re, 0, Some(max), mode) {
  override def replicate(re: RE) = copy(re)
}

/** Zero-or-one (`?`) quantifier */
case class Opt(override val re: RE, override val mode: RepMode = Greedy)
extends Rep(re, 0, Some(1), mode) {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) =
    linear(_ + "?" + mode, groupNames)
}

/** Zero-or-more (`+`) quantifier */
case class KStar(override val re: RE, override val mode: RepMode = Greedy)
extends Rep(re, 0, mode = mode) {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) =
    linear(_ + "*" + mode, groupNames)
}

/** One-or-more (`+`) quantifier */
case class KCross(override val re: RE, override val mode: RepMode = Greedy)
extends Rep(re, 1, mode = mode) {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) =
    linear(_ + "+" + mode, groupNames)
}

/** Utility all-purpose subtree wrapper.
 *
 *  Should mainly be used to implement Flavors / tree transformations.
 */
case class Wrapper(
  override val re: RE,
  val prefix: String,
  val suffix: String,
  appendGroupNames: List[String] = Nil)
extends RE1(re) {
  override def replicate(re: RE) = copy(re)

  override def linear(groupNames: List[String]) =
    linear(prefix + _ + suffix, groupNames ::: appendGroupNames)
}


/** RE tree leaf */
sealed abstract class RE0 extends RE {
  def linear(groupNames: List[String]) =
    (toString, groupNames)

  override lazy val groups = Nil

  override def /:[A](z: A)(op: OpRewriter[A], order: RE.TraversalOrder): (RE, A) =
    op(z, this)

  override def foreach[U](order: RE.TraversalOrder)(f: RE => U) {
    f(this)
  }
}

/** Reference on a (named) capturing group */
case class GroupRef(val name: String, val embedStyle: Option[GroupNamingStyle] = None)
extends RE0 with Wrapped {

  override def linear(groupNames: List[String]) =
    (embedStyle.map(_ reference name).getOrElse("\\" + (groupNames.lastIndexOf(name) + 1)), groupNames)

  lazy val unary_! = this
}

/** Standalone raw regex expression (not interpreted) for external instanciation */
case class Atom(val re: Regex) extends RE0 {
  override def linear(groupNames: List[String]) =
    (re.toString, groupNames)
}

/** Escaped (litteral) expression */
case class Escaped(val value: String) extends RE0 {
  def this(s: Symbol) = this(s.toString.substring(1))

  lazy val reStr = RE.escapeRegex(value)
  override def toString = reStr
}

/** Abstract standalone raw regex expression (not interpreted) */
abstract class REStr(val reStr: String) extends RE0 {
  override def toString = reStr
}

/** Abstract standalone raw regex expression that don't need non-capturing group protection. */
abstract class RECst(val reCst: String) extends REStr(reCst) with Wrapped

/** A litteral integer */
case class DigitCst(val i: Int)
extends RECst(if (i < 10) i.toString else "(?:" + i.toString + ")")
