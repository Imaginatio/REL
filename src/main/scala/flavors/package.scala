package fr.splayce.rel

import util.{Flavor, FlavorLike, Rewriter}


package object flavors {

  /** Utility no-op Rewriter, bypasses recusion */
  val IdRewriter: Rewriter = { case re => re }

}

package flavors {

  class TranslatedREStr(val s: String) extends REStr(s)
  class TranslatedRECst(val s: String) extends RECst(s)

  /** Translates a Possessive quantifier into a Greedy one wrapped into an AtomicGroup: `a++` becomes `(?>a+)`.
   *
   *  Strict equivalent for flavors that don't support possessive repeaters.
   */
  trait PossessiveToAtomic extends FlavorLike {
    protected lazy val noPtoA = super.tr

    override lazy val tr: Rewriter = {
      val pToA: Rewriter = {
        case r: Rep if (r.mode == Possessive) => PossessiveToAtomic.rewrite(tr)(r)
      }
      pToA orElse super.tr
    }
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
    override lazy val tr: Rewriter = {
      val aToLA: Rewriter = {
        case r: AGroup                        => AtomicToLookAhead.rewrite(tr)(r)
        // possessive repeaters => atomic group => previous case
        case r: Rep if (r.mode == Possessive) => tr(PossessiveToAtomic.rewrite(IdRewriter)(r))
      }
      aToLA orElse noPtoA
    }
  }
  object AtomicToLookAhead {
    def rewrite(translate: Rewriter)(ag: AGroup): RE = {
      val g = (ag.re map translate).g
      (?=(g) - !g).ncg
    }
  }

}
