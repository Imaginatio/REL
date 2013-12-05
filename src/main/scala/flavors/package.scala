package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.{Flavor, FlavorLike, Rewriter, IdRewriter, RecursiveIdRewriter, OpRewriter}
import TraversalOrder.{Prefixed, InfixedPre}

import scala.util.matching.Regex



/** Java ≤ 6 Flavor. Strips embedded group names and named references. */
object Java6Flavor extends Flavor("Java 6") with StripGroupNames

/** Java ≥ 7 Flavor. Embeds strict group names and named references. */
object Java7Flavor extends Flavor("Java 7") with EmbedGroupNames {
  override val groupNamingStyle = ChevNamingStyle
  override val groupNamingValidator = RE.strictGroupName
}

/** PCRE Flavor (C, PHP, Ruby 1.9 / Oniguruma…).
 *
 *  Embeds group names and named references (snake-case),
 *  and uses `\R` short syntax for `LineTerminator`. */
object PCREFlavor extends Flavor("PCRE") with EmbedGroupNames {
  private val SimpleLineTerminator = new TranslatedRECst("""\R""")
  override val translator: Rewriter = {
    case LineTerminator => SimpleLineTerminator
  }
}


class TranslatedREStr(val s: String) extends REStr(s)
class TranslatedRECst(val s: String) extends RECst(s)


/** Embeds validly-named capturing groups and references, Java 7-style (`(?<name>expr)` and `(\k<name>)`).
 *
 *  Keeps Scala-style group names at Regex instanciation.
 *
 *  Valid inline group names are those matching the overridable
 *  `groupNamingValidator` regex (`RE.snakeGroupName` by default),
 *  unique if `groupNamingUnicity` if left to `true`.
 *  This validation is post-checked, so that an extending Flavor's
 *  `translator` can alter group names beforehand.
 *
 *  @see java.util.regex.Pattern
 *  @see scala.util.matching.Regex
 *  @see fr.splayce.rel.flavors.GroupNameSimplifier
 */
trait EmbedGroupNames extends FlavorLike {
  val groupNamingStyle: GroupNamingStyle = ChevNamingStyle
  val groupNamingValidator: Regex = RE.snakeGroupName
  val groupNamingUnicity: Boolean = true

  lazy val egn: Rewriter = {
    case Group(name, re, ns) if ns != Some(groupNamingStyle) =>
      Group(name, re, Some(groupNamingStyle))
    case GroupRef(name, ns) if ns != Some(groupNamingStyle) =>
      GroupRef(name, Some(groupNamingStyle))
  }

  lazy val validateGroupNames: Rewriter = {
    case Group(name, re, Some(_)) if ! groupNamingValidator.pattern.matcher(name).matches
      => Group(name, re, None)
    case r @ GroupRef(name, Some(_)) if ! groupNamingValidator.pattern.matcher(name).matches
      => r.copy(embedStyle = None)
  }

  lazy val uniqueGroupNames: OpRewriter[List[String]] = { (names, re) =>
    re match {
      case Group(name, re, Some(_)) if names.contains(name) =>
        (Group(name, re, None), names ::: List(name))
      case g @ Group(name, _, Some(_)) =>
        (g, names ::: List(name))
      case GroupRef(name, Some(_)) if names.count(_ == name) > 1 =>
        (GroupRef(name, None), names)
      case _ => (re, names)
    }
  }

  override def translate(re: RE) = {
    var t = super.translate(re map egn)
    if (groupNamingUnicity) {
      t = (List.empty[String] /: t)(uniqueGroupNames, InfixedPre) _1
    }
    t map validateGroupNames
  }
}

/** Strips inline-named capturing groups and references, Java 6-style (`(expr)` and `(\n)`).
 *
 *  Keeps Scala-style group names at Regex instanciation.
 *  @see java.util.regex.Pattern
 *  @see scala.util.matching.Regex
 */
trait StripGroupNames extends FlavorLike {
  lazy val sgn: Rewriter = {
    case    Group(name, re, Some(_)) =>    Group(name, re, None)
    case GroupRef(name,     Some(_)) => GroupRef(name,     None)
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
    case r: Rep if (r.mode == Possessive) => PossessiveToAtomic.rewrite(r)
  }

  override def translate(re: RE) = noPtoA(re map pToA)
}
object PossessiveToAtomic {
  def rewrite(rep: Rep): RE = (rep: @unchecked) match {
    case         Opt(re,       Possessive) =>         Opt(re,       Greedy).ag
    case       KStar(re,       Possessive) =>       KStar(re,       Greedy).ag
    case      KCross(re,       Possessive) =>      KCross(re,       Greedy).ag
    case     RepNToM(re, n, m, Possessive) =>     RepNToM(re, n, m, Greedy).ag
    case RepAtLeastN(re, n,    Possessive) => RepAtLeastN(re, n,    Greedy).ag
    case  RepAtMostN(re, n,    Possessive) =>  RepAtMostN(re, n,    Greedy).ag
    case r: RepExactlyN                    => r
  }
}

/** Mimick atomic grouping and possessive quantifiers with capturing LookAheads.
 *
 *  Transforms atomic grouping and possessive quantifiers into a capturing
 *  LookAhead + immediate backreference: `(?>a)` becomes `(?:(?=(a))\1)` and
 *  `a++` becomes `(?:(?=(a+))\1)`
 *
 *  This permits the usage of atomic groups (and consequently, possessive quantifiers)
 *  in flavors that don't support them but do support LookAhead.
 *
 *  @note This trick uses a capturing group and hence alters the capturing groups list.
 *  @see [[fr.splayce.rel.flavors.PossessiveToAtomic]]
 */
trait AtomicToLookAhead extends PossessiveToAtomic {
  lazy val aToLA: Rewriter = {
    case r: AGroup                        => AtomicToLookAhead.rewrite(r)
    // possessive repeaters => atomic group => previous case
    case r: Rep if (r.mode == Possessive) => aToLA(PossessiveToAtomic.rewrite(r))
  }

  override def translate(re: RE) = noPtoA(re map aToLA)
}
object AtomicToLookAhead {
  def rewrite(ag: AGroup): RE = {
    val g = (ag.re).g
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

  override def translate(re: RE) = super.translate(re.map(unicodeCategories, Prefixed))
}

/** Prevents usage of Unicode: single characters, categories. */
trait NoUnicodeSupport extends NoUnicodeCategoriesSupport {
  private val ASCIILineTerminator = new TranslatedRECst("""(?:\r\n?|\n)""")

  val unicode: Rewriter = {
    // Skip unicode in LineTerminator
    case LineTerminator => ASCIILineTerminator
  }

  override def translate(re: RE) = super.translate(re.map(unicode, Prefixed))
}


/** Prevents usage of LookBehind. */
trait NoLookBehindSupport extends FlavorLike {
  val lookBehind: Rewriter = {
    case LookAround(_, Behind, _) => notSupported("LookBehind", false)
  }

  override def translate(re: RE) = super.translate(re.map(lookBehind, Prefixed))
}

/** Prevents usage of LookAround. */
trait NoLookAroundSupport extends NoLookBehindSupport {
  val lookAround: Rewriter = {
    case la: LookAround => notSupported("LookAround", false)
  }

  override def translate(re: RE) = super.translate(re.map(lookAround, Prefixed))
}

/** Prevents usage of local mode modifiers (NCG flags like `(?-i:sub-expression)`) */
trait NoLocalModeModifierSupport extends FlavorLike {
  val localModeModifiers: Rewriter = {
    case NCGroup(_, flags) if flags != "" => notSupported("Local mode modifiers (NCG flags)", true)
  }

  override def translate(re: RE) = super.translate(re.map(localModeModifiers, Prefixed))
}
