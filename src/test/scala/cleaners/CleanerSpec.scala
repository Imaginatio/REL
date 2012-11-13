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
      DiacriticCleaner.diacritics.toList
        .filter({p => DiacriticCleaner.nfdClean(p._1.toString) == p._2}) must be empty
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
    class PrefixingCleaner(val prefix: String) extends Cleaner {
      override def clean(in: String) = prefix + in
    }
    
    val aPrefix = new PrefixingCleaner("a ")
    val bPrefix = new PrefixingCleaner("b ")
    val cPrefix = new PrefixingCleaner("c ")
    val chain  = cPrefix | bPrefix | aPrefix
    val chain2 = aPrefix(bPrefix(cPrefix))

    "perform all cleaners in order" in {
      chain("test")  must_== "a b c test"
      chain2("test") must_== "a b c test"
    }

    "have equivalent chaining syntaxes (functions vs. pipes)" in {
      chain2 must_== chain
    }
  }

}