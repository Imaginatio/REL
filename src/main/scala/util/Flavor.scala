package fr.splayce.REL.util

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.REL._


abstract class Flavor {

  def express(re: RE): (String, List[String]) =
    translate(re).linear()

  /** Returns a recursively translation of the given RE tree.
    *
    * This is the method to override when implementing a Flavor;
    * the default case being typically:
    * {{{
    * case _ => super.translate(re)
    * }}}
    */
  def translate(re: RE): RE = re match {

    case Wrapper(re, p, s, n) => Wrapper(translate(re), p, s, n)

    // repeaters
    case         Opt(re,       mode) =>         Opt(translate(re),       mode)
    case       KStar(re,       mode) =>       KStar(translate(re),       mode)
    case      KCross(re,       mode) =>      KCross(translate(re),       mode)
    case     RepNToM(re, n, m, mode) =>     RepNToM(translate(re), n, m, mode)
    case RepAtLeastN(re, n,    mode) => RepAtLeastN(translate(re), n,    mode)
    case  RepAtMostN(re, n,    mode) =>  RepAtMostN(translate(re), n,    mode)
    case RepExactlyN(re, n         ) => RepExactlyN(translate(re), n         )

    // other RE1 (grouping)
    case Group(name, re)       => Group(name, translate(re))
    case      AGroup(re)       =>      AGroup(translate(re))
    case     NCGroup(re)       =>     NCGroup(translate(re))
    case  LookAround(re, d, p) =>  LookAround(translate(re), d, p)

    // RE2
    case Conc(re1, re2) => Conc(translate(re1), translate(re2))
    case  Alt(re1, re2) =>  Alt(translate(re1), translate(re2))

    case _ => re
  }
}
