package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.Rewriter


/** @see [[fr.splayce.rel.flavors.JavaScriptFlavor]]
  * @todo variant for XRegExp with Unicode http://xregexp.com/plugins/#unicode
  */
object JavaScriptTranslator {

  val translate: Rewriter = {

    // JavaScript regexes are pretty limited...
    case LookAround(_, Behind, _)         => notSupported("LookBehind", false)
    case r: Rep if (r.mode == Possessive) => notSupported("Possessive quantifiers", true)
    case AGroup(_)                        => notSupported("Atomic grouping", false)

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

  private val notSupported = unsupported("JavaScript")(_, _)

}