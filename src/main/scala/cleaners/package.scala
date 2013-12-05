package fr.splayce.rel.cleaners

import fr.splayce.rel.util.{Cleaner, TrackString}
import Cleaner.regexReplaceAll
import TrackString._

import scala.util.matching.Regex.Match


/** No-op cleaner */
object IdentityCleaner extends Cleaner(s => s)

/** Transform text in lowercase */
object LowerCaseFilter extends Cleaner(_ toLowerCase)

/** Trim text */
object TrimFilter extends Cleaner(_ trim, { in =>
  val start = in.current.indexWhere(_ > ' ')
  val end = in.current.lastIndexWhere(_ > ' ') + 1
  val len = in.current.length

  if (start == 0 && end == len) in
  else {
    var repl = if (end == len) Repl() else Repl(end, len, end, end)
    if (start > 0) repl = repl + Repl(0, start, 0, 0)
    in.edit(in.current.substring(start, end), repl)
  }
})

/** Normalize all Unicode line breaks and vertical tabs to ASCII new line `U+000A` / `\n`.
 *  @see [[http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Bidi_Class=Paragraph_Separator:] Unicode: Paragraph Separator]],
 *       [[http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Line_Break=Mandatory_Break:] Unicode: Mandatory Break]]
 */
object LineSeparatorNormalizer extends Cleaner(regexReplaceAll(
  """(?>\r\n|[\u000A-\u000D\u001C-\u001E\u0085\u2028\u2029])""".r, "\n"))
/** Normalize all Unicode spaces and horizontal tabs to ASCII spaces `U+0020`.
 *  @see [[http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:General_Category=Space_Separator:] Unicode: Space Separator]],
 *       [[http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:Bidi_Class=Segment_Separator:] Unicode: Segment Separator]]
 */
object WhiteSpaceNormalizer extends Cleaner(regexReplaceAll(
  """[\u0009\u001F\u0020\u00A0\u180E\u1680\u2000-\u200A\u202F\u205F\u3000]""".r, " "))

/** Replace multiple instances of regular whitespaces `\s+` by a single space.
 *  @see [[http://docs.oracle.com/javase/6/docs/api/java/util/regex/Pattern.html#predef Java Regex: Predefined character classes]]
 */
object WhiteSpaceCleaner extends Cleaner(regexReplaceAll("""\s+""".r, " "))
/** Replace multiple instances of all Unicode whitespaces by a single space.
 *  A faster equivalent to `LineSeparatorNormalizer | WhiteSpaceNormalizer | WhiteSpaceCleaner`.
 *  @see [[http://unicode.org/cldr/utility/list-unicodeset.jsp?a=[:White_Space=Yes:] Unicode: White Space]]
 */
object AllWhiteSpaceCleaner extends Cleaner(regexReplaceAll(
  """[\u0009-\u000D\u001C-\u001F\u0020\u0085\u00A0\u180E\u1680\u2000-\u200A\u2028\u2029\u202F\u205F\u3000]+""".r, " "))

/** Split CamelCase words.
 *
 *  The split occurs only on the form `aBc` (lower-upper-lower).
 *  Will therefore split `someWords` but not `iOS` nor `VitaminC`.
 */
object CamelCaseSplitFilter extends Cleaner(regexReplaceAll("""(\p{Ll})(?=\p{Lu}\p{Ll})""".r, "$1 "))

/** Normalize frequent Unicode single quotes / apostrophes to ASCII apostrophe `U+0027` / `'`. */
object SingleQuoteNormalizer extends Cleaner(regexReplaceAll("[‘’＇′‵]".r, "'"))
/** Normalize frequent Unicode double quotes to ASCII quotation mark `U+0022` / `"`. */
object DoubleQuoteNormalizer extends Cleaner(regexReplaceAll("[“”＂″‶〝〞]".r, "\""))
/** Combines SingleQuoteNormalizer and DoubleQuoteNormalizer */
object QuoteNormalizer extends Cleaner(SingleQuoteNormalizer | DoubleQuoteNormalizer)

/** Pseudo ASCII folding, remove diacritical marks (and some common variants and ligatures) on characters. */
object DiacriticCleaner extends Cleaner(DiacriticFolder.clean _, { in =>
  import DiacriticFolder._
  in.replaceAll(unicodeMarked, "$1")   // remove diacritics already in NFD
    .edit(s => (nfdClean(s), Repl()))  // nfdClean does not cause shifting
    .replaceAll(diacritics.get _)      // per-char replacement
})

/** Normalize CJK Fullwidth characters to their ASCII equivalents. */
object FullwidthNormalizer extends Cleaner({
  val re = "[\\uFF01-\\uFF5E￠￡￤￥￦]+".r
  val singles = Map('￠'->'¢', '￡'->'£', '￥'->'¥', '￦'->'₩')

  val convert: Char => Char = (c: Char) =>
    if (c < '\uFF5F') ((c.toInt & 0xFF) + 0x20) toChar
    else singles.getOrElse(c, c)

  val convertMatch = { (m: Match) => m.matched map convert replaceAllLiterally("$", "\\$") }

  { (in: String) => re.replaceAllIn(in, convertMatch) }
})
