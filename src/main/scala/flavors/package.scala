package fr.splayce.rel

import util.Flavor
import util.Rewriter


package object flavors {

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
  

}
