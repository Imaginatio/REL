package fr.splayce.REL.flavors

import fr.splayce.REL._
import util.Flavor

object JavaScriptFlavor extends Flavor {

  override def translate(re: RE): RE = re match {

    // JavaScript regexes are pretty limited...
    case LookAround(_, Behind, _)         => notSupported("LookBehind")
    case r: Rep if (r.mode == Possessive) => notSupported("Possessive quantifiers", true)
    case AGroup(_)                        => notSupported("Atomic grouping")

    // Javascript doesn't support Unicode categories natively
    // although one may use XRegExp with Unicode plugin:
    // http://xregexp.com/plugins/#unicode
    case LetterLower => notSupported("Unicode categories (including LetterLower)", true)
    case LetterUpper => notSupported("Unicode categories (including LetterUpper)", true)
    case Letter      => notSupported("Unicode categories (including Letter)",      true)
    case NotLetter   => notSupported("Unicode categories (including NotLetter)",   true)

    // this needs the 'm' flag not to be specified
    case InputBeginning => LineBeginning
    case InputEnd       => LineEnd

    case _ => super.translate(re)
  }

  private def notSupported(what: String, plural: Boolean = false) =
    throw new IllegalArgumentException((what
        :: (if (plural) "are" else "is")
        :: "not supported in JavaScript" :: Nil) mkString " ")

}