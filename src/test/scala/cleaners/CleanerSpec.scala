package fr.splayce.REL.cleaners

import org.specs2.mutable._

import fr.splayce.REL.util.Cleaner


class CleanerSpec extends Specification {

  "Identity Cleaner" should {
    "leave string untouched" in {
      val dirty = " dirty  ''.+string"
      IdentityCleaner(dirty) must_== dirty
    }
  }

  "WhiteSpace Cleaner" should {
    "Remove multiple whitespaces" in {
      WhiteSpaceCleaner("  double  spaces  ") must_== " double spaces "
    }
    "Translate other whitespaces to spaces" in {
      WhiteSpaceCleaner("list:\n\tfirst\r\n\tsecond") must_== "list: first second"
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

  "CamelCase Splitter" should {
    "split CamelCase words" in {
      CamelCaseSplitter("someWords in CamelCase") must_== "some Words in Camel Case"
    }
    "ignore double uppercase" in {
      CamelCaseSplitter("NOtCAmelCAse") must_== "NOtCAmelCAse"
    }
    "ignore trailing uppercase" in {
      CamelCaseSplitter("TestT") must_== "TestT"
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