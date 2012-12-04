package fr.splayce.rel

import fr.splayce.rel.util.Cleaner

import scala.util.matching.Regex.Match


package object cleaners {

  /** No-op cleaner */
  val IdentityCleaner = Cleaner { in => in }

  /** Transform text in lowercase */
  val LowerCaseFilter = Cleaner { _ toLowerCase }

  /** Trim text */
  val TrimFilter = Cleaner { _ trim }

  /** Normalize all Unicode spaces and horizontal tabs to ASCII new line `U+000A` / `\n`.
    * @see http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Bidi_Class=Paragraph_Separator:]
    * @see http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Line_Break=Mandatory_Break:]
    */
  val LineSeparatorNormalizer = Cleaner.regexReplaceAll(
    """(?>\r\n|[\u000A-\u000D\u001C-\u001E\u0085\u2028\u2029])""".r, "\n")
  /** Normalize all Unicode spaces and horizontal tabs to ASCII spaces `U+0020`.
    * @see http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:General_Category=Space_Separator:]
    * @see http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Bidi_Class=Segment_Separator:]
    */
  val WhiteSpaceNormalizer = Cleaner.regexReplaceAll(
    """[\u0009\u001F\u0020\u00A0\u180E\u1680\u2000-\u200A\u202F\u205F\u3000]""".r, " ")

  /** Replace multiple instances of regular whitespaces `\s+` by a single space.
    * @see http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#predef
    */
  val WhiteSpaceCleaner = Cleaner.regexReplaceAll("""\s+""".r, " ")
  /** Replace multiple instances of all Unicode whitespaces by a single space.
    * A faster equivalent to `LineSeparatorNormalizer | WhiteSpaceNormalizer | WhiteSpaceCleaner`.
    * @see http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:White_Space=Yes:]
    */
  val AllWhiteSpaceCleaner = Cleaner.regexReplaceAll(
    """[\u0009-\u000D\u001C-\u001F\u0020\u0085\u00A0\u180E\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]+""".r, " ")

  /** Split CamelCase words.
    *
    * The split occurs only on the form `aBc` (lower-upper-lower).
    * Will therefore split `someWords` but not `iOS` nor `VitaminC`.
    */
  val CamelCaseSplitter = Cleaner.regexReplaceAll("""(\p{Ll})(?=\p{Lu}\p{Ll})""".r, "$1 ")

  /** Normalize frequent Unicode single quotes / apostrophes to ASCII apostrophe `U+0027` / `'`. */
  val SingleQuoteNormalizer = Cleaner.regexReplaceAll("[‘’＇′‵]".r, "'")
  /** Normalize frequent Unicode double quotes to ASCII quotation mark `U+0022` / `"`. */
  val DoubleQuoteNormalizer = Cleaner.regexReplaceAll("[“”＂″‶〝〞]".r, "\"")
  /** Combines SingleQuoteNormalizer and DoubleQuoteNormalizer */
  val QuoteNormalizer = SingleQuoteNormalizer | DoubleQuoteNormalizer

  /** Pseudo ASCCI folding, remove diacritical marks (and some common variants) on characters. */
  val DiacriticCleaner = Cleaner(DiacriticFolder.clean _)

  /** Normalize CJK Fullwidth characters to their ASCII equivalents. */
  val FullwidthNormalizer = {
    val re = "[\\uFF01-\\uFF5E￠￡￤￥￦]+".r
    val singles = Map('￠'->'¢', '￡'->'£', '￥'->'¥', '￦'->'₩')

    def convert(c: Char): Char =
      if (c < '\uFF5F') ((c.toInt & 0xFF) + 0x20) toChar
      else singles.getOrElse(c, c)

    val convertMatch = { (m: Match) => m.matched map convert replaceAllLiterally("$", "\\$") }

    Cleaner { in => re.replaceAllIn(in, convertMatch) }
  }

}
