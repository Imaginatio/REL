package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.{Flavor, Rewriter}

  /** Vanilla JavaScript / ECMA-262 flavor, with very limited Unicode support
   *
   *  This flavor:
   *  - translates `^^` / `InputBegin` and `$$` / `InputEnd` to
   *    `^` / `LineBegin` and `$` / `LineEnd` respectively
   *    (this won't work if the `m` flag is set)
   *  - transform possessive quantifiers and atomic groups (also not supported in JavaScript)
   *    into LookAhead with capturing group, immediately referenced afterwards:
   *    `(?>a|b)` becomes `(?=(a|b))\1`, exposing the same behavior
   *  - throws an error when using unsupported features:
   *    - LookBehind
   *    - Unicode categories
   *
   *  @see [[fr.splayce.rel.flavors.AtomicToLookAhead]]
   *  @see [[http://www.regular-expressions.info/javascript.html JavaScript regex flavor]]
   *  @see [[http://xregexp.com/plugins/#unicode XRegExp Unicode extension]] for better Unicode support
   *  @note The AtomicToLookAhead may add additional, possibly unwanted capturing groups to mimick atomic grouping
   *  @todo variant for XRegExp with Unicode http://xregexp.com/plugins/#unicode
   */
object JavaScriptFlavor
extends Flavor("JavaScript")
with StripGroupNames
with AtomicToLookAhead
with NoUnicodeCategoriesSupport
with NoLookBehindSupport {

  override val translator: Rewriter = {

    // this needs the 'm' flag not to be specified
    case InputBegin => LineBegin
    case InputEnd   => LineEnd

  }

}
