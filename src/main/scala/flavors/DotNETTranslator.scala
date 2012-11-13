package fr.splayce.REL.flavors

import fr.splayce.REL._
import util.Rewriter


object DotNETTranslator {

  class Cst(val c: String) extends RECst(c)

  val translate: Rewriter = {

    // named groups & their references
    case    Group(name, re) => Wrapper(re map translate, "(?<" + name + ">", ")", List(name))
    case GroupRef(name)     => new Cst("""\k<""" + name + ">")

    // no possessive repeater but can be translated
    // to greedy repeater in an atomic group
    case         Opt(re,        Possessive) =>         Opt(re map translate,       Greedy).ag
    case       KStar(re,        Possessive) =>       KStar(re map translate,       Greedy).ag
    case      KCross(re,        Possessive) =>      KCross(re map translate,       Greedy).ag
    case     RepNToM(re, n, m,  Possessive) =>     RepNToM(re map translate, n, m, Greedy).ag
    case RepAtLeastN(re, n,     Possessive) => RepAtLeastN(re map translate, n,    Greedy).ag
    case  RepAtMostN(re, n,     Possessive) =>  RepAtMostN(re map translate, n,    Greedy).ag

    // .NET's \w would match letters with diacritics
    case Word    => new Cst("[a-zA-Z0-9_]")
    case NotWord => new Cst("[^a-zA-Z0-9_]")

  }

}