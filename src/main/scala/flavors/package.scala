package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.{Flavor, FlavorLike, Rewriter, IdRewriter, RecursiveIdRewriter}



/** Java ≤ 6 Flavor. Strips embedded group names and named references. */
object Java6Flavor extends Flavor("Java 6") with StripGroupNames

/** Java ≥ 7 Flavor. Embeds group names and named references. */
object Java7Flavor extends Flavor("Java 7") with EmbedGroupNames {
  override val groupNamingStyle = ChevNamingStyle
}

/** PCRE Flavor. Embeds group names and named references, using P-style. */
object PCREFlavor extends Flavor("PCRE") with EmbedGroupNames {
  override val groupNamingStyle = PNamingStyle
}


class TranslatedREStr(val s: String) extends REStr(s)
class TranslatedRECst(val s: String) extends RECst(s)


/** Embeds named capturing groups and references, Java 7-style (`(?<name>expr)` and `(\k<name>)`).
 *
 * Keeps Scala-style group names at Regex instanciation.
 * @see [[java.util.regex.Pattern]]
 * @see [[scala.util.matching.Regex]]
 */
trait EmbedGroupNames extends FlavorLike {
  val groupNamingStyle: GroupNamingStyle

  lazy val egn: Rewriter = {
    case Group(name, re, ns) if ns != Some(groupNamingStyle) =>
      Group(name, re map egn, Some(groupNamingStyle))
    case GroupRef(name, ns) if ns != Some(groupNamingStyle) =>
      GroupRef(name, Some(groupNamingStyle))
  }

  override def translate(re: RE) = super.translate(re map egn)
}

/** Strips inline-named capturing groups and references, Java 6-style (`(expr)` and `(\n)`).
 *
 * Keeps Scala-style group names at Regex instanciation.
 * @see [[java.util.regex.Pattern]]
 * @see [[scala.util.matching.Regex]]
 */
trait StripGroupNames extends FlavorLike {
  lazy val sgn: Rewriter = {
    case    Group(name, re, Some(_)) =>    Group(name, re map sgn, None)
    case GroupRef(name,     Some(_)) => GroupRef(name,             None)
  }

  override def translate(re: RE) = super.translate(re map sgn)
}


/** Translates a Possessive quantifier into a Greedy one wrapped into an AtomicGroup: `a++` becomes `(?>a+)`.
 *
 *  Strict equivalent for flavors that don't support possessive repeaters.
 */
trait PossessiveToAtomic extends FlavorLike {
  protected def noPtoA(re: RE) = super.translate(re)

  lazy val pToA: Rewriter = {
    case r: Rep if (r.mode == Possessive) => PossessiveToAtomic.rewrite(pToA)(r)
  }

  override def translate(re: RE) = noPtoA(re map pToA)
}
object PossessiveToAtomic {
  def rewrite(translate: Rewriter)(rep: Rep): RE = (rep: @unchecked) match {
    case         Opt(re,        Possessive) =>         Opt(re map translate,       Greedy).ag
    case       KStar(re,        Possessive) =>       KStar(re map translate,       Greedy).ag
    case      KCross(re,        Possessive) =>      KCross(re map translate,       Greedy).ag
    case     RepNToM(re, n, m,  Possessive) =>     RepNToM(re map translate, n, m, Greedy).ag
    case RepAtLeastN(re, n,     Possessive) => RepAtLeastN(re map translate, n,    Greedy).ag
    case  RepAtMostN(re, n,     Possessive) =>  RepAtMostN(re map translate, n,    Greedy).ag
  }
}

/** Mimick atomic grouping and possessive quantifiers with capturing LookAheads.
 *
 * Transforms atomic grouping and possessive quantifiers into a capturing
 * LookAhead + immediate backreference: `(?>a)` becomes `(?:(?=(a))\1)` and
 * `a++` becomes `(?:(?=(a+))\1)`
 *
 *  This permits the usage of atomic groups (and consequently, possessive quantifiers)
 *  in flavors that don't support them but do support LookAhead.
 *
 *  @note This trick uses a capturing group and hence alters the capturing groups list.
 *  @see [[fr.splayce.rel.flavors.PossessiveToAtomic]]
 */
trait AtomicToLookAhead extends PossessiveToAtomic {
  lazy val aToLA: Rewriter = {
    case r: AGroup                        => AtomicToLookAhead.rewrite(aToLA)(r)
    // possessive repeaters => atomic group => previous case
    case r: Rep if (r.mode == Possessive) => aToLA(PossessiveToAtomic.rewrite(IdRewriter)(r))
  }

  override def translate(re: RE) = noPtoA(re map aToLA)
}
object AtomicToLookAhead {
  def rewrite(translate: Rewriter)(ag: AGroup): RE = {
    val g = (ag.re map translate).g
    (?=(g) - !g).ncg
  }
}


/** Prevents usage of Unicode categories. */
trait NoUnicodeCategoriesSupport extends FlavorLike {
  val unicodeCategories: Rewriter = {
    case LetterLower => notSupported("Unicode categories (including LetterLower)", true)
    case LetterUpper => notSupported("Unicode categories (including LetterUpper)", true)
    case Letter      => notSupported("Unicode categories (including Letter)",      true)
    case NotLetter   => notSupported("Unicode categories (including NotLetter)",   true)
  }

  override def translate(re: RE) = super.translate(re map unicodeCategories)
}

/** Prevents usage of Unicode: single characters, categories. */
trait NoUnicodeSupport extends NoUnicodeCategoriesSupport {
  private val ASCIILineTerminator = new TranslatedRECst("""(?:\r\n?|\n)""")

  val unicode: Rewriter = {
    // Skip unicode in LineTerminator
    case LineTerminator => ASCIILineTerminator
  }

  override def translate(re: RE) = super.translate(re map unicode)
}


/** Prevents usage of LookBehind. */
trait NoLookBehindSupport extends FlavorLike {
  val lookBehind: Rewriter = {
    case LookAround(_, Behind, _) => notSupported("LookBehind", false)
  }

  override def translate(re: RE) = super.translate(re map lookBehind)
}

/** Prevents usage of LookAround. */
trait NoLookAroundSupport extends NoLookBehindSupport {
  val lookAround: Rewriter = {
    case la: LookAround => notSupported("LookAround", false)
  }

  override def translate(re: RE) = super.translate(re map lookAround)
}
