package fr.splayce.REL

import fr.splayce.REL.util.Cleaner

import scala.util.matching.Regex.Match


package object cleaners {

  val IdentityCleaner = Cleaner { in => in }

  val LowerCaseFilter = Cleaner { _ toLowerCase }

  val WhiteSpaceCleaner = Cleaner.regexReplaceAll("""\s+""".r, " ")

  val CamelCaseSplitter = Cleaner.regexReplaceAll("""(\p{Ll})(?=\p{Lu}\p{Ll})""".r, "$1 ")

  val SingleQuoteNormalizer = Cleaner.regexReplaceAll("[‘’＇′‵]".r, "'")
  val DoubleQuoteNormalizer = Cleaner.regexReplaceAll("[“”＂″‶〝〞]".r, "\"")
  val QuoteNormalizer = SingleQuoteNormalizer | DoubleQuoteNormalizer

  val DiacriticCleaner = Cleaner(DiacriticFolder.clean _)

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
