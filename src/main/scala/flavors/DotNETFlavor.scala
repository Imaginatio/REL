package fr.splayce.REL.flavors

import fr.splayce.REL._
import util.Flavor

object DotNETFlavor extends Flavor {

  class Cst(val c: String) extends RECst(c)
  
  
  override def translate(re: RE): RE = re match {

    // named groups & their references
    case    Group(name, re) => Wrapper(translate(re), "(?<" + name + ">", ")", List(name))
    case GroupRef(name)     => new Cst("""\k<""" + name + ">")

    // no possessive repeater but can be translated
    // to greedy repeater in an atomic group
    case         Opt(re,        Possessive) =>         Opt(translate(re),       Greedy).ag
    case       KStar(re,        Possessive) =>       KStar(translate(re),       Greedy).ag
    case      KCross(re,        Possessive) =>      KCross(translate(re),       Greedy).ag
    case     RepNToM(re, n, m,  Possessive) =>     RepNToM(translate(re), n, m, Greedy).ag
    case RepAtLeastN(re, n,     Possessive) => RepAtLeastN(translate(re), n,    Greedy).ag
    case  RepAtMostN(re, n,     Possessive) =>  RepAtMostN(translate(re), n,    Greedy).ag
    
    // .NET's \w would match diacritics
    case Word    => new Cst("[a-zA-Z0-9_]")
    case NotWord => new Cst("[^a-zA-Z0-9_]")

    case _ => super.translate(re)
  }

}