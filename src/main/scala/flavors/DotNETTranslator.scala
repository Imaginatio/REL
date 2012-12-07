package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.Rewriter


/** @see [[fr.splayce.rel.flavors.DotNETFlavor]] */
object DotNETTranslator {

  private val ASCIIWord    = new TranslatedRECst("[a-zA-Z0-9_]")
  private val NotASCIIWord = new TranslatedRECst("[^a-zA-Z0-9_]")

  lazy val translate: Rewriter = {

    // named groups & their references
    case    Group(name, re) => Wrapper(re map translate, "(?<" + name + ">", ")", List(name))
    case GroupRef(name)     => new TranslatedREStr("""\k<""" + name + ">")

    // .NET's \w would also match letters with diacritics
    case Word               => ASCIIWord
    case NotWord            => NotASCIIWord

    // Also, no possessive quantifiers
    case rep: Rep if rep.mode == Possessive => possessiveToAtomic(translate)(rep)
  }

}