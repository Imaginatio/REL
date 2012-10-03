package fr.splayce.REL.cleaners

import fr.splayce.REL.util.Cleaner
import java.text.Normalizer


object DiacriticCleaner extends Cleaner {

  override def clean(in: String): String =
    nfdClean(in) flatMap diacriticFold

  def nfdClean(in: String) = // "è" => unicode normalized decomposition "e`" => "e"
    unicodeMarks.replaceAllIn(Normalizer.normalize(in, Normalizer.Form.NFD), "")

  def diacriticFold(c: Char): List[Char] =
    // OPTIMIZE cache list creation for frequently used letters ?
    if (diacritics contains c) diacritics(c).toList
    else c :: Nil

  val unicodeMarks = "\\p{IsM}+".r

  val diacritics = Map(
    '０'->"0", '₀'->"0", '⓪'->"0", '⁰'->"0",
    '¹'->"1", '⑴'->"1", '₁'->"1", '❶'->"1", '⓵'->"1", '⒈'->"1", '①'->"1", '１'->"1",
    '❷'->"2", '⑵'->"2", '２'->"2", '₂'->"2", '⓶'->"2", '②'->"2", '⒉'->"2", '²'->"2",
    '３'->"3", '³'->"3", '⒊'->"3", '⑶'->"3", '₃'->"3", '❸'->"3", '⓷'->"3", '③'->"3",
    '⓸'->"4", '④'->"4", '⒋'->"4", '４'->"4", '⁴'->"4", '₄'->"4", '❹'->"4", '⑷'->"4",
    '⒌'->"5", '₅'->"5", '⓹'->"5", '⑸'->"5", '❺'->"5", '⑤'->"5", '５'->"5", '⁵'->"5",
    '⑹'->"6", '⁶'->"6", '６'->"6", '❻'->"6", '₆'->"6", '⑥'->"6", '⓺'->"6", '⒍'->"6",
    '７'->"7", '⁷'->"7", '❼'->"7", '⓻'->"7", '⒎'->"7", '₇'->"7", '⑺'->"7", '⑦'->"7",
    '⑧'->"8", '⒏'->"8", '⓼'->"8", '⑻'->"8", '⁸'->"8", '８'->"8", '❽'->"8", '₈'->"8",
    '⓽'->"9", '９'->"9", '⒐'->"9", '❾'->"9", '⑼'->"9", '₉'->"9", '⑨'->"9", '⁹'->"9",
    'Ⱥ'->"A", 'Ａ'->"A", 'Ɐ'->"A", 'Ⓐ'->"A",
    'Ꜳ'->"AA", 'Ǽ'->"AE", 'Æ'->"AE", 'Ǣ'->"AE", 'Ꜵ'->"AO", 'Ꜷ'->"AU", 'Ꜹ'->"AV", 'Ꜻ'->"AV", 'Ꜽ'->"AY",
    'Ɓ'->"B", 'Ｂ'->"B", 'Ƀ'->"B", 'Ƃ'->"B", 'Ⓑ'->"B",
    'Ⓒ'->"C", 'Ƈ'->"C", 'Ȼ'->"C", 'Ꜿ'->"C", 'Ｃ'->"C",
    'Ⓓ'->"D", 'Ｄ'->"D", 'Ꝺ'->"D", 'Ɖ'->"D", 'Ɗ'->"D", 'Đ'->"D", 'Ƌ'->"D",
    'Ǆ'->"DZ", 'Ǳ'->"DZ", 'ǅ'->"Dz", 'ǲ'->"Dz",
    'Ｅ'->"E", 'Ǝ'->"E", 'Ɛ'->"E", 'Ⓔ'->"E",
    'Ｆ'->"F", 'Ⓕ'->"F", 'Ƒ'->"F", 'Ꝼ'->"F",
    'Ꞡ'->"G", 'Ǥ'->"G", 'Ɠ'->"G", 'Ᵹ'->"G", 'Ⓖ'->"G", 'Ꝿ'->"G", 'Ｇ'->"G",
    'Ⓗ'->"H", 'Ⱨ'->"H", 'Ｈ'->"H", 'Ɥ'->"H", 'Ⱶ'->"H", 'Ħ'->"H",
    'Ɨ'->"I", 'Ｉ'->"I", 'Ⓘ'->"I",
    'Ɉ'->"J", 'Ｊ'->"J", 'Ⓙ'->"J",
    'Ƙ'->"K", 'Ꝃ'->"K", 'Ꝁ'->"K", 'Ꞣ'->"K", 'Ꝅ'->"K", 'Ⓚ'->"K", 'Ｋ'->"K", 'Ⱪ'->"K",
    'Ꝇ'->"L", 'Ｌ'->"L", 'Ł'->"L", 'Ꞁ'->"L", 'Ⱡ'->"L", 'Ꝉ'->"L", 'Ƚ'->"L", 'Ŀ'->"L", 'Ɫ'->"L", 'Ⓛ'->"L",
    'Ǉ'->"LJ", 'ǈ'->"Lj",
    'Ⓜ'->"M", 'Ｍ'->"M", 'Ɯ'->"M", 'Ɱ'->"M",
    'Ⓝ'->"N", 'Ꞑ'->"N", 'Ɲ'->"N", 'Ｎ'->"N", 'Ƞ'->"N", 'Ꞥ'->"N",
    'Ǌ'->"NJ", 'ǋ'->"Nj",
    'Ꝍ'->"O", 'Ø'->"O", 'Ⓞ'->"O", 'Ꝋ'->"O", 'Ɔ'->"O", 'Ǿ'->"O", 'Ɵ'->"O", 'Ｏ'->"O",
    'Œ'->"OE", 'Ƣ'->"OI", 'Ꝏ'->"OO", 'Ȣ'->"OU",
    'Ꝑ'->"P", 'Ｐ'->"P", 'Ꝕ'->"P", 'Ᵽ'->"P", 'Ƥ'->"P", 'Ꝓ'->"P", 'Ⓟ'->"P",
    'Ꝙ'->"Q", 'Ɋ'->"Q", 'Ｑ'->"Q", 'Ꝗ'->"Q", 'Ⓠ'->"Q", 'Ⓡ'->"R",
    'Ꞃ'->"R", 'Ɽ'->"R", 'Ꞧ'->"R", 'Ｒ'->"R", 'Ꝛ'->"R", 'Ɍ'->"R",
    'Ꞩ'->"S", 'Ｓ'->"S", 'Ⓢ'->"S", 'Ꞅ'->"S", 'Ȿ'->"S",
    'ẞ'->"SS",
    'Ⱦ'->"T", 'Ƭ'->"T", 'Ꞇ'->"T", 'Ʈ'->"T", 'Ｔ'->"T", 'Ⓣ'->"T", 'Ŧ'->"T",
    'Ꜩ'->"TZ",
    'Ｕ'->"U", 'Ⓤ'->"U", 'Ʉ'->"U",
    'Ｖ'->"V", 'Ⓥ'->"V", 'Ꝟ'->"V", 'Ʋ'->"V", 'Ʌ'->"V",
    'Ꝡ'->"VY",
    'Ｗ'->"W", 'Ⓦ'->"W", 'Ⱳ'->"W",
    'Ｘ'->"X", 'Ⓧ'->"X", 'Ｙ'->"Y", 'Ƴ'->"Y", 'Ỿ'->"Y", 'Ɏ'->"Y", 'Ⓨ'->"Y",
    'Ⱬ'->"Z", 'Ｚ'->"Z", 'Ⓩ'->"Z", 'Ꝣ'->"Z", 'Ȥ'->"Z", 'Ɀ'->"Z", 'Ƶ'->"Z",
    'ⓐ'->"a", 'ẚ'->"a", 'ａ'->"a", 'ⱥ'->"a", 'ɐ'->"a",
    'ꜳ'->"aa", 'æ'->"ae", 'ǽ'->"ae", 'ǣ'->"ae", 'ꜵ'->"ao", 'ꜷ'->"au", 'ꜹ'->"av", 'ꜻ'->"av", 'ꜽ'->"ay",
    'ƀ'->"b", 'ｂ'->"b", 'ⓑ'->"b", 'ƃ'->"b", 'ɓ'->"b",
    'ƈ'->"c", 'ⓒ'->"c", 'ↄ'->"c", 'ȼ'->"c", 'ꜿ'->"c", 'ｃ'->"c",
    'ƌ'->"d", 'ɖ'->"d", 'ｄ'->"d", 'ɗ'->"d", 'ꝺ'->"d", 'ⓓ'->"d", 'đ'->"d",
    'ǳ'->"dz", 'ǆ'->"dz",
    'ⓔ'->"e", 'ｅ'->"e", 'ɇ'->"e", 'ǝ'->"e", 'ɛ'->"e",
    'ꝼ'->"f", 'ⓕ'->"f", 'ƒ'->"f", 'ｆ'->"f",
    'ǥ'->"g", 'ᵹ'->"g", 'ꞡ'->"g", 'ɠ'->"g",
    'ⓖ'->"g", 'ｇ'->"g", 'ꝿ'->"g",
    'ⱶ'->"h", 'ｈ'->"h", 'ⱨ'->"h", 'ħ'->"h", 'ɥ'->"h", 'ⓗ'->"h",
    'ƕ'->"hv",
    'ⓘ'->"i", 'ｉ'->"i", 'ɨ'->"i", 'ı'->"i",
    'ⓙ'->"j", 'ｊ'->"j", 'ɉ'->"j",
    'ꞣ'->"k", 'ꝃ'->"k", 'ⱪ'->"k", 'ꝅ'->"k", 'ƙ'->"k", 'ｋ'->"k", 'ⓚ'->"k", 'ꝁ'->"k",
    'ŀ'->"l", 'ɫ'->"l", 'ｌ'->"l", 'ꝇ'->"l", 'ꞁ'->"l", 'ⱡ'->"l", 'ⓛ'->"l", 'ꝉ'->"l", 'ƚ'->"l", 'ł'->"l",
    'ǉ'->"lj",
    'ɯ'->"m", 'ｍ'->"m", 'ɱ'->"m", 'ⓜ'->"m",
    'ⓝ'->"n", 'ŉ'->"n", 'ꞑ'->"n", 'ɲ'->"n", 'ƞ'->"n", 'ꞥ'->"n", 'ｎ'->"n",
    'ǌ'->"nj",
    'ɵ'->"o", 'ǿ'->"o", 'ø'->"o", 'ꝋ'->"o", 'ⓞ'->"o", 'ｏ'->"o", 'ɔ'->"o", 'ꝍ'->"o",
    'œ'->"oe", 'ƣ'->"oi", 'ꝏ'->"oo", 'ȣ'->"ou",
    'ｐ'->"p", 'ᵽ'->"p", 'ꝕ'->"p", 'ƥ'->"p", 'ꝑ'->"p", 'ⓟ'->"p", 'ꝓ'->"p",
    'ɋ'->"q", 'ⓠ'->"q", 'ꝙ'->"q", 'ꝗ'->"q", 'ｑ'->"q",
    'ꞧ'->"r", 'ꞃ'->"r", 'ɽ'->"r", 'ｒ'->"r", 'ɍ'->"r", 'ꝛ'->"r", 'ⓡ'->"r",
    'ſ'->"s", 'ｓ'->"s", 'ꞅ'->"s", 'ꞩ'->"s", 'ⓢ'->"s", 'ȿ'->"s",
    'ß'->"ss",
    'ⓣ'->"t", 'ꞇ'->"t", 'ƭ'->"t", 'ｔ'->"t", 'ʈ'->"t", 'ŧ'->"t", 'ⱦ'->"t",
    'ꜩ'->"tz",
    'ⓤ'->"u", 'ｕ'->"u", 'ʉ'->"u",
    'ʋ'->"v", 'ｖ'->"v", 'ⓥ'->"v", 'ʌ'->"v", 'ꝟ'->"v",
    'ꝡ'->"vy",
    'ｗ'->"w", 'ⱳ'->"w", 'ⓦ'->"w",
    'ⓧ'->"x", 'ｘ'->"x",
    'ỿ'->"y", 'ｙ'->"y", 'ƴ'->"y", 'ɏ'->"y", 'ⓨ'->"y",
    'ꝣ'->"z", 'ⱬ'->"z", 'ｚ'->"z", 'ƶ'->"z", 'ɀ'->"z", 'ⓩ'->"z", 'ȥ'->"z"
  )

}
