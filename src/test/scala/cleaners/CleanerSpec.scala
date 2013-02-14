package fr.splayce.rel.cleaners

import org.specs2.mutable._

import fr.splayce.rel.util.Cleaner


class CleanerSpec extends Specification {

  "Identity Cleaner" should {
    "leave string untouched" in {
      val dirty = " dirty  ''.+string"
      IdentityCleaner(dirty) must_== dirty
    }
  }

  "WhiteSpace Cleaner" should {
    "Remove multiple ASCII whitespaces" in {
      WhiteSpaceCleaner("  double  spaces  ") must_== " double spaces "
    }
    "Translate other ASCII whitespaces to spaces" in {
      WhiteSpaceCleaner("list:\n\tfirst\r\n\tsecond") must_== "list: first second"
    }
  }
  "WhiteSpace Normalizers" should {
    "Translate Unicode white spaces and tabs to ASCII spaces" in {
      WhiteSpaceNormalizer(" \u0009\u001F\u0020\u00A0\u180E\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000")
        .must_==("                     ")
    }
    "Translate Unicode line separators to ASCII new lines" in {
      LineSeparatorNormalizer("\r\n \u000A\u000B\u000C\u000D\u001C\u001D\u001E\u0085\u2028\u2029")
        .must_==("\n \n\n\n\n\n\n\n\n\n\n")
    }
  }
  "All WhiteSpace Cleaner" should {
    "Remove multiple Unicode whitespaces and line separators" in {
      AllWhiteSpaceCleaner("  double  spaces  ") must_== " double spaces "
      AllWhiteSpaceCleaner("A" + " \u0009\u001F\u0020\u00A0\u180E\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\r\n \u000A\u000B\u000C\u000D\u001C\u001D\u001E\u0085\u2028\u2029"
          + "B") must_== "A B"
    }
  }

  "Quote Normalizer" should {
    "Convert special quotes" in {
      QuoteNormalizer("She said “I’ll see you soon”.") must_== """She said "I'll see you soon"."""
      QuoteNormalizer("‘’＇′‵") must_== "'''''"
      QuoteNormalizer("“”＂″‶〝〞") must_== "\"\"\"\"\"\"\""
    }
  }

  "Diacritic Cleaner" should {
    "remove diacritics and other alternations" in {
      DiacriticCleaner("０₀⓪⁰¹⑴₁❶⓵⒈①１❷⑵２₂⓶②⒉²３³⒊⑶₃❸⓷③⓸④⒋４⁴₄❹⑷⒌₅⓹⑸❺⑤５⁵⑹⁶６❻₆⑥⓺⒍７⁷❼⓻⒎₇⑺⑦⑧⒏⓼⑻⁸８❽₈⓽９⒐❾⑼₉⑨⁹")
        .must_== ("0000111111112222222233333333444444445555555566666666777777778888888899999999")
      DiacriticCleaner("ẮẪȦẲẰȺÄẶȂǠȀĄÀÅẢǍĀẦÁＡẴⱯẬⒶÂẤĂẠḀǞǺÃẨƁＢḆḂɃƂḄⒷⒸḈĊĈƇÇȻꜾĆＣČḐⒹĎＤḌḒꝹƉƊḊĐƋḎȆĔÊＥĘÉẾỂỆḖƎẺḚḘĒȨĖȄẸĚỄËỀḔẼƐⒺḜÈＦḞⒻƑꝻǴĠĜꞠǤƓꝽⒼꝾĞＧĢḠǦⒽḤȞⱧḪĤＨḢꞍḦḨⱵĦḮÎỊĮƗȊＩÍĪĨÌȈḬǏⒾİÏĬỈĴɈＪⒿƘꝂḰꝀḲǨꞢꝄⓀĶＫⱩḴꝆḶＬŁꞀĽⱠḺꝈȽḸĹĿĻⱢⓁḼⓂＭƜḾṂⱮṀⓃṄǸṊŅṆꞐƝŃＮÑŇȠṈꞤỒꝌỐỎƠØǬỖÓṐṎŎⓄỢṒỞỚȪȎÔȮꝊÕỔỌǪỘṌŐǑȬỠȌƆǾÖÒƟŌＯỜȰṖꝐＰꝔⱣƤꝒⓅṔꝘɊＱꝖⓆŔŘⓇꞂṞⱤꞦṚＲȒṘŖꝚɌṜȐŠꞨＳŜṦⓈŞṢṨŚꞄȘⱾṤṠȚṮṰṪȾƬŤꞆƮＴṬⓉŦŢṶÙỮỪǛÜŮȖǓṲǗŨＵṺỰỦⓊṸÛŪỬƯǙɄŲṴỤÚŰỨȔǕŬＶⓋṾꝞƲɅṼＷŴⓌⱲẆẂẈẀẄẊẌＸⓍỲＹẎŸỶÝƳỾɎȲỴⓎỸŶŻẐŽⱫＺⓏꝢẒẔŹȤⱿƵ")
        .must_== ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBCCCCCCCCCCCDDDDDDDDDDDDDEEEEEEEEEEEEEEEEEEEEEEEEEEEEEFFFFFGGGGGGGGGGGGGGHHHHHHHHHHHHHIIIIIIIIIIIIIIIIIIIJJJJKKKKKKKKKKKKKLLLLLLLLLLLLLLLLLMMMMMMMNNNNNNNNNNNNNNNOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOPPPPPPPPPQQQQQRRRRRRRRRRRRRRRRSSSSSSSSSSSSSSSTTTTTTTTTTTTTTUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUVVVVVVVWWWWWWWWWXXXXYYYYYYYYYYYYYYZZZZZZZZZZZZZ")
      DiacriticCleaner("ǟǡⓐāằäąấǻẩẳẚåȧậàáạȃặȁａăảⱥǎâḁầắẫãẵɐƀḃｂⓑḇƃḅɓčĉƈⓒↄȼḉꜿċćçｃḋḑḏƌɖｄďɗḓꝺḍⓓđêềⓔếḕễẻｅḛéḗēɇȇėḝęểẹǝẽȩḙệȅëɛěĕèꝼḟⓕƒｆġǥᵹḡĝꞡģɠⓖǧｇꝿğǵⱶĥẖḥｈⱨḩḧħɥḣⓗḫȟȋĭⓘîḭỉĩｉíìɨįȉīịıḯïǐⓙｊɉĵǰḱꞣḵǩķꝃⱪḳꝅƙｋⓚꝁŀɫļｌḻḷꝇḽꞁľⱡⓛḹĺꝉƚłḿɯṁｍɱⓜṃⓝŉńꞑňṅṉṇɲǹñƞꞥｎņṋɵōȫởỡǿơṑȯớóȏøọṍȱǭồꝋỗôṓǒŏợõờộȍȭⓞốöỏṏｏòɔőǫổꝍｐᵽꝕƥꝑṗⓟꝓṕɋⓠꝙꝗｑṛꞧṟꞃȑɽŗｒřɍȓṝṙꝛŕⓡśṥſẛｓšŝṡșṩṧꞅꞩṣⓢşȿṱⓣťꞇƭṭẗｔʈţŧțⱦṫṯŭùǔȕǘṻụüũṷửⓤǜｕứųṳʉůȗûủṹǖựữṵúūưǚừűʋｖṿṽⓥʌꝟｗẅẁⱳẉẇẃẘŵⓦⓧẍｘẋỿｙƴɏýỳỷⓨŷẙỹȳÿỵẏꝣⱬẑżẓｚžƶɀⓩźȥẕ")
        .must_== ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbccccccccccccdddddddddddddeeeeeeeeeeeeeeeeeeeeeeeeeeeeeefffffgggggggggggggghhhhhhhhhhhhhhiiiiiiiiiiiiiiiiiiijjjjjkkkkkkkkkkkkklllllllllllllllllmmmmmmmnnnnnnnnnnnnnnnnoooooooooooooooooooooooooooooooooooooooooopppppppppqqqqqrrrrrrrrrrrrrrrrssssssssssssssssstttttttttttttttuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuvvvvvvvwwwwwwwwwwxxxxyyyyyyyyyyyyyyyzzzzzzzzzzzzz")
    }
    "dissociate combined letters" in {
      DiacriticCleaner("ꜲǼÆǢꜴꜶꜸꜺꜼǄǱǅǲǇǈǊǋŒƢꝎȢẞꜨꝠꜳæǽǣꜵꜷꜹꜻꜽǳǆƕǉǌœɶƣꝏȣßꜩꝡ")
        .must_== ("AAAEAEAEAOAUAVAVAYDZDZDzDzLJLjNJNjOEOIOOOUSSTZVYaaaeaeaeaoauavavaydzdzhvljnjoeoeoiooousstzvy")
    }
    "not hold unnecessary chars in direct translation map" in {
      DiacriticFolder.diacritics.toList
        .filter({p => DiacriticFolder.nfdClean(p._1.toString) == p._2}) must be empty
    }
  }

  "Fullwidth Normalizer" should {
    "normalize Fullwidth to ASCII" in {
      FullwidthNormalizer("！＂＃＄％＆＇（）＊＋，－．／０１２３４５６７８９：；＜＝＞？＠ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ［＼］＾＿｀ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ｛｜｝～")
        .must_==("!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")
      FullwidthNormalizer("￠￡￥￦")
        .must_==("¢£¥₩")
    }
  }

  "CamelCase Split Filter" should {
    "split CamelCase words" in {
      CamelCaseSplitFilter("someWords in CamelCase") must_== "some Words in Camel Case"
    }
    "ignore double uppercase" in {
      CamelCaseSplitFilter("NOtCAmelCAse") must_== "NOtCAmelCAse"
    }
    "ignore trailing uppercase" in {
      CamelCaseSplitFilter("TestT") must_== "TestT"
    }
  }

  "Cleaner Chain" should {

    def suffixingCleaner(suffix: String) = Cleaner(_ + suffix)

    val aSuffix = suffixingCleaner(" a")
    val bSuffix = suffixingCleaner(" b")
    val cSuffix = suffixingCleaner(" c")

    val chain1 = aSuffix | bSuffix | cSuffix
    val chain2 = cSuffix(  bSuffix(  aSuffix))

    "perform all cleaners in order" in {
      chain1("test") must_== "test a b c"
      chain2("test") must_== "test a b c"
    }
  }

}