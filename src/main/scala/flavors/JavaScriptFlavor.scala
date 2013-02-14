package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.{Flavor, Rewriter}

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
   *  @todo variant for XRegExp with Unicode http://xregexp.com/plugins/#unicode
   */
object JavaScriptFlavor extends Flavor("JavaScript") {

  override lazy val translator: Rewriter = {

    // JavaScript regexes are pretty limited...
    case LookAround(_, Behind, _)         => notSupported("LookBehind", false)
    // Atomic Grouping is not supported but we can emulate this with capturing LookAhead
    case r: AGroup                        => atomicToLookAhead(translator)(r)
    // same goes for possessive repeaters => atomic group => previous case
    case r: Rep if (r.mode == Possessive) => translator(possessiveToAtomic(IdRewriter)(r))

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

}