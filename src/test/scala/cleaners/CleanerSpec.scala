package fr.splayce.rel.cleaners

import org.specs2.mutable._

import fr.splayce.rel.util.{Cleaner, TrackString}
import TrackString._


class CleanerSpec extends Specification {

  "Identity Cleaner" should {
    "leave string untouched" in {
      val dirty = " dirty  ''.+string"
      IdentityCleaner(dirty) must_== dirty
    }
    "edit TrackString: no-op" in {
      val ts = TrackString("str")
      val its = IdentityCleaner(ts)
      its must_== ts
      its.repl must_== Repl()
      its.srcPos(0) must_== Interval(0, 1)
      its.srcPos(1) must_== Interval(1, 2)
      its.srcPos(2) must_== Interval(2, 3)
    }
  }

  "Default TrackString cleaning" should {
    "register no shift in position when length remains the same" in {
      val c = Cleaner(_ toUpperCase)
      val ts = c(TrackString("test"))
      ts.repl must_== Repl()
    }
    "register whole-String when length remains the same" in {
      val c = Cleaner(_ + "!")
      val os = "test"
      val ts = c(TrackString(os))
      ts.repl must_== Repl(0, os.length, 0, ts.current.length)
    }
  }

  "Trim Filter" should {
    "trim string" in {
      TrimFilter(" str")  must_== "str"
      TrimFilter("str ")  must_== "str"
      TrimFilter(" str ") must_== "str"
    }
    "edit TrackString: deletion" in {
      var ts = TrimFilter(TrackString(" str"))
      ts.toString must_== "str"
      ts.repl must_== Repl(0, 1, 0, 0)
      ts.srcPos(0) must_== Interval(1, 2)
      ts.srcPos(2) must_== Interval(3, 4)

      ts = TrimFilter(TrackString("str "))
      ts.toString must_== "str"
      ts.repl must_== Repl(3, 4, 3, 3)
      ts.srcPos(0) must_== Interval(0, 1)
      ts.srcPos(2) must_== Interval(2, 3)

      ts = TrimFilter(TrackString(" str "))
      ts.toString must_== "str"
      ts.repl must_== Repl(Subst(4, 5, 3, 3) :: Subst(0, 1, 0, 0) :: Nil)
      ts.srcPos(0) must_== Interval(1, 2)
      ts.srcPos(2) must_== Interval(3, 4)
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
    "edit TrackString: same-length replacement, compression" in {
      val ts = AllWhiteSpaceCleaner(TrackString("  A \n\t B\u2000"))
      ts.toString must_== " A B "
      ts.repl must_== Repl(Subst(8, 9, 4, 5) :: Subst(3, 7, 2, 3) :: Subst(0, 2, 0, 1) :: Nil)
      ts.srcPos(0) must_== Interval(0, 2)
      ts.srcPos(1) must_== Interval(2, 3)
      ts.srcPos(2) must_== Interval(3, 7)
      ts.srcPos(3) must_== Interval(7, 8)
      ts.srcPos(4) must_== Interval(8, 9)
    }
  }

  "Quote Normalizer" should {
    "Convert special quotes" in {
      QuoteNormalizer("She said “I’ll see you soon”.") must_== """She said "I'll see you soon"."""
      QuoteNormalizer("‘’＇′‵") must_== "'''''"
      QuoteNormalizer("“”＂″‶〝〞") must_== "\"\"\"\"\"\"\""
    }
    "edit TrackString: same-length replacement" in {
      val ts = QuoteNormalizer(TrackString("“Don’t”"))
      ts.toString must_== "\"Don't\""
      ts.repl must_== Repl(Subst(6, 7, 6, 7) :: Subst(4, 5, 4, 5) :: Subst(0, 1, 0, 1) :: Nil)
      ts.srcPos(0) must_== Interval(0, 1)
      ts.srcPos(4) must_== Interval(4, 5)
      ts.srcPos(6) must_== Interval(6, 7)
    }
  }

  "Diacritic Cleaner" should {
    "remove diacritics and other alternations" in {
      DiacriticCleaner("０₀⓪⁰¹⑴₁❶⓵⒈①１❷⑵２₂⓶②⒉²３³⒊⑶₃❸⓷③⓸④⒋４⁴₄❹⑷⒌₅⓹⑸❺⑤５⁵⑹⁶６❻₆⑥⓺⒍７⁷❼⓻⒎₇⑺⑦⑧⒏⓼⑻⁸８❽₈⓽９⒐❾⑼₉⑨⁹")
        .must_== ("0000111111112222222233333333444444445555555566666666777777778888888899999999")
      DiacriticCleaner("ẮẪȦẲẰȺÄẶȂǠȀĄÀÅẢǍĀẦÁＡẴⱯẬⒶÂẤĂẠḀǞǺÃẨƁＢḆḂɃƂḄⒷⒸḈĊĈƇÇȻꜾĆＣČḐⒹĎＤḌḒꝹƉƊḊĐƋḎȆĔÊＥĘÉẾỂỆḖƎẺḚḘĒȨĖȄẸĚỄËỀḔẼƐⒺḜÈＦḞⒻƑꝻǴĠĜꞠǤƓꝽⒼꝾĞＧĢḠǦⒽḤȞⱧḪĤＨḢꞍḦḨⱵĦḮÎỊĮƗȊＩÍĪĨÌȈḬǏⒾİÏĬỈĴɈＪⒿKƘꝂḰꝀḲǨꞢꝄⓀĶＫⱩḴꝆḶＬŁꞀĽⱠḺꝈȽḸĹĿĻⱢⓁḼⓂＭƜḾṂⱮṀⓃṄǸṊŅṆꞐƝŃＮÑŇȠṈꞤỒꝌỐỎƠØǬỖÓṐṎŎⓄỢṒỞỚȪȎÔȮꝊÕỔỌǪỘṌŐǑȬỠȌƆǾÖÒƟŌＯỜȰṖꝐＰꝔⱣƤꝒⓅṔꝘɊＱꝖⓆŔŘⓇꞂṞⱤꞦṚＲȒṘŖꝚɌṜȐŠꞨＳŜṦⓈŞṢṨŚꞄȘⱾṤṠȚṮṰṪȾƬŤꞆƮＴṬⓉŦŢṶÙỮỪǛÜŮȖǓṲǗŨＵṺỰỦⓊṸÛŪỬƯǙɄŲṴỤÚŰỨȔǕŬＶⓋṾꝞƲɅṼＷŴⓌⱲẆẂẈẀẄẊẌＸⓍỲＹẎŸỶÝƳỾɎȲỴⓎỸŶŻẐŽⱫＺⓏꝢẒẔŹȤⱿƵ")
        .must_== ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBCCCCCCCCCCCDDDDDDDDDDDDDEEEEEEEEEEEEEEEEEEEEEEEEEEEEEFFFFFGGGGGGGGGGGGGGHHHHHHHHHHHHHIIIIIIIIIIIIIIIIIIIJJJJKKKKKKKKKKKKKKLLLLLLLLLLLLLLLLLMMMMMMMNNNNNNNNNNNNNNNOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOPPPPPPPPPQQQQQRRRRRRRRRRRRRRRRSSSSSSSSSSSSSSSTTTTTTTTTTTTTTUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUUVVVVVVVWWWWWWWWWXXXXYYYYYYYYYYYYYYZZZZZZZZZZZZZ")
      DiacriticCleaner("ǟǡⓐāằäąấǻẩẳẚåȧậàáạȃặȁａăảⱥǎâḁầắẫãẵɐƀḃｂⓑḇƃḅɓčĉƈⓒↄȼḉꜿċćçｃḋḑḏƌɖｄďɗḓꝺḍⓓđêềⓔếḕễẻｅḛéḗēɇȇėḝęểẹǝẽȩḙệȅëɛěĕèꝼḟⓕƒｆġǥᵹḡĝꞡģɠⓖǧｇꝿğǵⱶĥẖḥｈⱨḩḧħɥḣⓗḫȟȋĭⓘîḭỉĩｉíìɨįȉīịıḯïǐⓙｊɉĵǰḱꞣḵǩķꝃⱪḳꝅƙｋⓚꝁŀɫļｌḻḷꝇḽꞁľⱡⓛḹĺꝉƚłḿɯṁｍɱⓜṃⓝńꞑňṅṉṇɲǹñƞꞥｎņṋɵōȫởỡǿơṑȯớóȏøọṍȱǭồꝋỗôṓǒŏợõờộȍȭⓞốöỏṏｏòɔőǫổꝍｐᵽꝕƥꝑṗⓟꝓṕɋⓠꝙꝗｑṛꞧṟꞃȑɽŗｒřɍȓṝṙꝛŕⓡśṥſẛｓšŝṡșṩṧꞅꞩṣⓢşȿṱⓣťꞇƭṭẗｔʈţŧțⱦṫṯŭùǔȕǘṻụüũṷửⓤǜｕứųṳʉůȗûủṹǖựữṵúūưǚừűʋｖṿṽⓥʌꝟｗẅẁⱳẉẇẃẘŵⓦⓧẍｘẋỿｙƴɏýỳỷⓨŷẙỹȳÿỵẏꝣⱬẑżẓｚžƶɀⓩźȥẕ")
        .must_== ("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabbbbbbbbccccccccccccdddddddddddddeeeeeeeeeeeeeeeeeeeeeeeeeeeeeefffffgggggggggggggghhhhhhhhhhhhhhiiiiiiiiiiiiiiiiiiijjjjjkkkkkkkkkkkkklllllllllllllllllmmmmmmmnnnnnnnnnnnnnnnoooooooooooooooooooooooooooooooooooooooooopppppppppqqqqqrrrrrrrrrrrrrrrrssssssssssssssssstttttttttttttttuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuvvvvvvvwwwwwwwwwwxxxxyyyyyyyyyyyyyyyzzzzzzzzzzzzz")
    }
    "dissociate combined letters" in {
      DiacriticCleaner("ꜲǼÆǢꜴꜶꜸꜺꜼǄǱǅǲĲǇǈǊǋŒƢꝎȢẞꜨꝠꜳæǽǣꜵꜷꜹꜻꜽǳǆﬀﬁﬂﬃﬄƕĳǉŉǌœɶƣꝏȣßﬆﬅꜩᵫꝡ")
        .must_== ("AAAEAEAEAOAUAVAVAYDZDZDzDzIJLJLjNJNjOEOIOOOUSSTZVYaaaeaeaeaoauavavaydzdzfffiflffifflhvijlj'nnjoeoeoioooussststtzuevy")
    }
    "not hold unnecessary chars in direct translation map" in {
      DiacriticFolder.diacritics.toList
        .filter({p => DiacriticFolder.nfdClean(p._1.toString) == p._2}) must be empty
    }
    "edit TrackString: compression, same-length replacement, expansion" in {
      // \u0065\u0301 is NFD of 'é' as 'e' + '◌́'
      val ts = DiacriticCleaner(TrackString("Test\u0065\u0301 à l'œil"))
      ts.toString must_== "Teste a l'oeil"
      ts.repl must_== Repl(Subst(11, 12, 10, 12) :: Subst(4, 6, 4, 5) :: Nil)
      // Subst(7, 8, 6, 7) is not recorded (does not cause shift)
      ts.srcPos(3) must_== Interval(3, 4)
      ts.srcPos(4) must_== Interval(4, 6)
      ts.srcPos(5) must_== Interval(6, 7)
      ts.srcPos(6) must_== Interval(7, 8)
      ts.srcPos(7) must_== Interval(8, 9)
      ts.srcPos(8) must_== Interval(9, 10)
      ts.srcPos(10) must_== Interval(11, 12)
      ts.srcPos(11) must_== Interval(11, 12)
      ts.srcPos(12) must_== Interval(12, 13)
    }
  }

  "Fullwidth Normalizer" should {
    "normalize Fullwidth to ASCII" in {
      FullwidthNormalizer("！＂＃＄％＆＇（）＊＋，－．／０１２３４５６７８９：；＜＝＞？＠ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ［＼］＾＿｀ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ｛｜｝～")
        .must_==("!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~")
      FullwidthNormalizer("￠￡￥￦")
        .must_==("¢£¥₩")
    }
    "add no replacement to TrackString" in {
      val ts = FullwidthNormalizer(TrackString("\uFF01\uFF5Eaa￠￡bb￤￥cc￦dd$ee"))
      ts.toString must_== "!~aa¢£bb￤¥cc₩dd$ee"
      ts.repl     must_== Repl()
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

    def suffixingCleaner(suffix: String) = Cleaner(_ + suffix, { in =>
      val l = in.current.length
      in.edit(in.current + suffix, Repl(l, l, l, l + suffix.length)) })

    val aSuffix = suffixingCleaner(" a")
    val bSuffix = suffixingCleaner(" b")
    val cSuffix = suffixingCleaner(" c")

    val chain1 = aSuffix | bSuffix | cSuffix
    val chain2 = cSuffix(  bSuffix(  aSuffix))

    "perform all cleaners in order" in {
      chain1("test") must_== "test a b c"
      chain2("test") must_== "test a b c"
    }

    "accumulate edit in TrackString" in {
      val ts = chain1(TrackString("test"))
      ts.toString  must_== "test a b c"
      ts.repl      must_== Repl(4, 4, 4, 10)
      ts.srcPos(0) must_== Interval(0, 1)
      ts.srcPos(3) must_== Interval(3, 4)
      ts.srcPos(4) must_== Interval(4, 4)
      ts.srcPos(5) must_== Interval(4, 4)
      ts.srcPos(6) must_== Interval(4, 4)
      ts.srcPos(7) must_== Interval(4, 4)
      ts.srcPos(8) must_== Interval(4, 4)
      ts.srcPos(9) must_== Interval(4, 4)
      ts.srcPos(0, 4)  must_== Interval(0, 4)
      ts.srcPos(4, 10) must_== Interval(4, 4)
      ts.srcPos(0, 10) must_== Interval(0, 4)
    }
  }

}