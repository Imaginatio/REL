package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.Rewriter


/** @see [[fr.splayce.rel.flavors.LegacyRubyFlavor]]
 *  @todo add endianness/charset variations that translate Unicode into multibytes
 */
object LegacyRubyTranslator {

  private val ASCIILineTerminator = new TranslatedRECst("""(?:\r\n?|\n)""")

  lazy val translate: Rewriter = {
    // Ruby regexes don't support LookBehind
    case LookAround(_, Behind, _) => notSupported("LookBehind", false)

    // Ruby doesn't support Unicode categories
    case LetterLower => notSupported("Unicode categories (including LetterLower)", true)
    case LetterUpper => notSupported("Unicode categories (including LetterUpper)", true)
    case Letter      => notSupported("Unicode categories (including Letter)",      true)
    case NotLetter   => notSupported("Unicode categories (including NotLetter)",   true)

    // Skip unicode in LineTerminator
    case LineTerminator => ASCIILineTerminator

    // Also, no possessive quantifiers
    case rep: Rep if rep.mode == Possessive => possessiveToAtomic(translate)(rep)
  }

  private val notSupported = unsupported("Legacy Ruby")(_, _)

}