package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.Rewriter


object JavaScriptTranslator {

  val translate: Rewriter = {

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
    case InputBegin => LineBegin
    case InputEnd   => LineEnd

  }

  private def notSupported(what: String, plural: Boolean = false) =
    throw new IllegalArgumentException((what
        :: (if (plural) "are" else "is")
        :: "not supported in JavaScript" :: Nil) mkString " ")

}