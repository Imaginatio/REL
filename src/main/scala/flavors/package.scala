package fr.splayce.rel

import util.Flavor
import util.Rewriter


package object flavors {

  /**  ․NET flavor
   *
   *  This flavor:
   *  - embedds group names in regex (`(?<name>expr)` / `\k<name>` syntax)
   *  - convert possessive quantifiers to greedy in atomic groups
   *  - translate `\w` (when referenced by `μ` / `Word`) into `[]`
   *    because .NET's `\w` would also matches letters with diacritics
   *    while Java's `\w` only matches ASCII letters
   *    (use `\p{L}` insead with `λ` / `Letter` for all Unicode letters)
   */
  val DotNETFlavor     = Flavor(DotNETTranslator    .translate)
  /** Vanilla JavaScript / ECMAScript flavor, with very limited Unicode support
   *
   *  This flavor:
   *  - translates `^^` / `InputBegin` and `$$` / `InputEnd` to
   *    `^` / `LineBegin` and `$` / `LineEnd` respectively
   *    (this won't work if the `m` flag is set)
   *  - throws an error when using unsupported features:
   *    - LookBehind
   *    - Possessive quantifiers
   *    - Atomic groups
   *    - Unicode categories
   *
   *  @see [[http://xregexp.com/plugins/#unicode XRegExp Unicode extension]] for better Unicode support
   */
  val JavaScriptFlavor = Flavor(JavaScriptTranslator.translate)
  /** Legacy Ruby <= 1․8 flavor, which does not support Unicode (at all) nor LookBehind.
   *
   *  For a wider support, it is recommended to use untranslated regexes instead,
   *  with [[http://www.geocities.jp/kosako3/oniguruma/ Oniguruma]],
   *  which supports LookBehind and Unicode when the `/u` flag is specified.
   *  Ruby 1.9 uses Oniguruma by default, Ruby 1.8 can be recompiled to use it.
   *
   *  This flavor:
   *  - removes Unicode variants in `Τ` / `LineTerminator`
   *  - convert possessive quantifiers to greedy in atomic groups
   *  - throws an error when using unsupported features:
   *    - LookBehind
   *    - Unicode categories
   *
   *  @see [[http://www.regular-expressions.info/ruby.html Ruby 1.8 regex flavor]],
   *       [[http://www.regular-expressions.info/unicode8bit.html Mixing Unicode and 8-bit Character Codes]]
   */
  val LegacyRubyFlavor = Flavor(LegacyRubyTranslator.translate)

  val IdRewriter: Rewriter = { case re => re }
  val PossessiveToAtomic: Rewriter = {
    case rep: Rep if rep.mode == Possessive => possessiveToAtomic(PossessiveToAtomic)(rep)
  }
  val AtomicToLookAhead: Rewriter = {
    case re: AGroup => atomicToLookAhead(AtomicToLookAhead)(re)
  }


  class TranslatedREStr(val s: String) extends REStr(s)
  class TranslatedRECst(val s: String) extends RECst(s)

  /** Translates a Possessive quantifier into a Greedy one wrapped into an AtomicGroup: `a++` becomes `(?>a+)`.
   *
   *  Strict equivalent for flavors that don't support possessive repeaters.
   *  @example To use it in a Rewriter named `translate`:
   *  {{{
   *  case rep: Rep if rep.mode == Possessive => possessiveToAtomic(translate)(rep)
   *  }}}
   */
  def possessiveToAtomic(translate: Rewriter)(rep: Rep): RE = (rep: @unchecked) match {
    case         Opt(re,        Possessive) =>         Opt(re map translate,       Greedy).ag
    case       KStar(re,        Possessive) =>       KStar(re map translate,       Greedy).ag
    case      KCross(re,        Possessive) =>      KCross(re map translate,       Greedy).ag
    case     RepNToM(re, n, m,  Possessive) =>     RepNToM(re map translate, n, m, Greedy).ag
    case RepAtLeastN(re, n,     Possessive) => RepAtLeastN(re map translate, n,    Greedy).ag
    case  RepAtMostN(re, n,     Possessive) =>  RepAtMostN(re map translate, n,    Greedy).ag
  }

  /** Transforms an atomic grouping into a capturing LookAhead + reference: `(?>a)` becomes `(?:(?=(a))\1)`.
   *
   *  This permits the usage of atomic groups (and consequently, possessive quantifiers)
   *  in flavors that don't support them but support LookAhead.
   *  @example You probably want to use this in combination with `possessiveToAtomic`.
   *  To use it in a Rewriter named `translate`:
   *  {{{
   *  case r: AGroup => atomicToLookAhead(translate)(r)
   *  case r: Rep if rep.mode == Possessive => translate(possessiveToAtomic(IdRewriter)(rep))
   *  }}}
   *  @note This trick uses a capturing group and hence alters the capturing groups list.
   */
  def atomicToLookAhead(translate: Rewriter)(ag: AGroup): RE = {
    val g = (ag.re map translate).g
    (?=(g) - !g).ncg
  }
  

  protected[flavors] def unsupported(flavor: String)(feature: String, plural: Boolean = false) =
    throw new IllegalArgumentException((feature
        :: (if (plural) "are" else "is")
        :: "not supported in" :: flavor :: Nil) mkString " ")

}
