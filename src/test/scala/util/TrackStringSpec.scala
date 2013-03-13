package fr.splayce.rel.util

import org.specs2.mutable._

import util.matching.Regex
import TrackString._

class TrackStringSpec extends Specification {

  "TrackString: Interval" should {
    "implement Allen's interval algebra" in {
      Interval(1, 2).before(Interval(3, 4)) must beTrue
      Interval(1, 2).before(Interval(2, 4)) must beFalse
      Interval(1, 2).meets(Interval(2, 3)) must beTrue
      Interval(1, 2).meets(Interval(1, 3)) must beFalse
      Interval(1, 2).meets(Interval(3, 4)) must beFalse
      Interval(1, 4).overlaps(Interval(2, 5)) must beTrue
      Interval(1, 3).overlaps(Interval(2, 4)) must beTrue
      Interval(1, 3).overlaps(Interval(1, 4)) must beFalse
      Interval(1, 3).overlaps(Interval(3, 4)) must beFalse
      Interval(1, 2).starts(Interval(1, 3)) must beTrue
      Interval(1, 2).starts(Interval(2, 3)) must beFalse
      Interval(1, 2).starts(Interval(1, 2)) must beFalse
      Interval(1, 2).starts(Interval(0, 3)) must beFalse
      Interval(1, 3).starts(Interval(2, 3)) must beFalse
      Interval(2, 3).during(Interval(1, 4)) must beTrue
      Interval(2, 3).during(Interval(2, 4)) must beFalse
      Interval(2, 3).during(Interval(1, 3)) must beFalse
      Interval(2, 3).finishes(Interval(1, 3)) must beTrue
      Interval(2, 3).finishes(Interval(2, 3)) must beFalse
      Interval(2, 3).finishes(Interval(1, 4)) must beFalse
      Interval(2, 5).contains(Interval(2, 5)) must beTrue
      Interval(2, 5).contains(Interval(2, 3)) must beTrue
      Interval(2, 5).contains(Interval(4, 5)) must beTrue
      Interval(2, 5).contains(Interval(3, 4)) must beTrue
      Interval(2, 5).contains(Interval(1, 2)) must beFalse
      Interval(2, 5).contains(Interval(5, 6)) must beFalse
      Interval(2, 5).contains(Interval(1, 3)) must beFalse
      Interval(2, 5).contains(Interval(4, 6)) must beFalse
    }
  }

  "TrackString: srcPos alone" should {
    "Support deletion aBc => ac" in {
      val res = TrackString("aBc", "ac", Repl(1, 2, 1, 1))
      res.srcPos(0) must_== Interval(0, 1)
      res.srcPos(1) must_== Interval(2, 3)
      val res2 = TrackString("aBCd", "ad", Repl(1, 3, 1, 1))
      res2.srcPos(0) must_== Interval(0, 1)
      res2.srcPos(1) must_== Interval(3, 4)
    }
    "Support insertion ac => aBc" in {
      val res = new TrackString("ac", "aBc", Repl(1, 1, 1, 2))
      res.srcPos(0) must_== Interval(0, 1)
      res.srcPos(1) must_== Interval(1, 1)
      res.srcPos(2) must_== Interval(1, 2)
      val res2 = new TrackString("ad", "aBCd", Repl(1, 1, 1, 3))
      res2.srcPos(0) must_== Interval(0, 1)
      res2.srcPos(1) must_== Interval(1, 1)
      res2.srcPos(2) must_== Interval(1, 1)
      res2.srcPos(3) must_== Interval(1, 2)
    }
    "Support same-length replace ab => cd" in {
      val res = new TrackString("_ab_", "_cd_", Repl(1, 3, 1, 3))
      res.srcPos(0) must_== Interval(0, 1)
      res.srcPos(1) must_== Interval(1, 3)
      res.srcPos(2) must_== Interval(1, 3)
      res.srcPos(3) must_== Interval(3, 4)
      val res2 = new TrackString("_abcd_", "_efgh_", Repl(1, 4, 1, 4))
      res2.srcPos(0) must_== Interval(0, 1)
      res2.srcPos(1) must_== Interval(1, 4)
      res2.srcPos(2) must_== Interval(1, 4)
      res2.srcPos(3) must_== Interval(1, 4)
      res2.srcPos(4) must_== Interval(4, 5)
    }
    "Support compression _abcd_ => _aEd_" in {
      val res = TrackString("_abcd_", "_aEd_", Repl(2, 4, 2, 3))
      res.srcPos(0) must_== Interval(0, 1)
      res.srcPos(1) must_== Interval(1, 2)
      res.srcPos(2) must_== Interval(2, 4)
      res.srcPos(3) must_== Interval(4, 5)
      res.srcPos(4) must_== Interval(5, 6)
    }
    "Support expansion _aed_ => _aBCd_" in {
      val res = TrackString("_aed_", "_aBCd_", Repl(2, 3, 2, 4))
      res.srcPos(0) must_== Interval(0, 1)
      res.srcPos(1) must_== Interval(1, 2)
      res.srcPos(2) must_== Interval(2, 3)
      res.srcPos(3) must_== Interval(2, 3)
      res.srcPos(4) must_== Interval(3, 4)
      res.srcPos(5) must_== Interval(4, 5)
    }
  }

  "TrackString: Subst#carved(Subst)" should {
    "Support beforeOrMeets" in {
      Subst(3, 4, 4, 5).carved(Subst(1, 3, 1, 5)) must_==(None, Some(Subst(3, 4, 4 + 2, 5 + 2)))
    }
    "Support overlapsOrStarts" in {
      Subst(3, 4, 2, 4).carved(Subst(1, 3, 1, 5)) must_==(None, Some(Subst(3, 4, 3 + 2, 4 + 2)))
    }
    "Support during" in {
      Subst(3, 4, 0, 4).carved(Subst(1, 3, 1, 5)) must_==(Some(Subst(3, 4, 0, 1)), Some(Subst(3, 4, 3 + 2, 4 + 2)))
    }
    "Support finishesOrIOverlaps" in {
      Subst(3, 4, 0, 2).carved(Subst(1, 3, 1, 5)) must_==(Some(Subst(3, 4, 0, 1)), None)
    }
    "Support IbeforeOrMeets" in {
      Subst(3, 4, 0, 1).carved(Subst(1, 3, 1, 5)) must_==(Some(Subst(3, 4, 0, 1)), None)
    }
    "Support other cases" in {
      Subst(3, 4, 1, 4).carved(Subst(1, 4, 1, 5)) must_==(None, None)
      Subst(3, 4, 1, 3).carved(Subst(1, 4, 1, 5)) must_==(None, None)
      Subst(3, 4, 2, 4).carved(Subst(1, 4, 1, 5)) must_==(None, None)
      Subst(3, 4, 2, 3).carved(Subst(1, 4, 1, 5)) must_==(None, None)
    }
  }

  "TrackString: Subst#carved(List[Subst])" should {
    "Support empty substs" in {
      Subst(1, 2, 1, 3).carved(Nil) must_==
        (List(Subst(1, 2, 1, 3)), Nil)
    }
    "Support beforeOrMeets" in {
      Subst(3, 4, 4, 5).carved(Subst(1, 3, 1, 5) :: Nil) must_==
        (List(Subst(3, 4, 4 + 2, 5 + 2)), List(Subst(Interval(1, 3), Interval(1, 5))))
    }
    "Support overlapsOrStarts" in {
      Subst(3, 4, 2, 4).carved(Subst(1, 3, 1, 5) :: Nil) must_==
        (List(Subst(3, 4, 3 + 2, 4 + 2)), List(Subst(Interval(1, 3), Interval(1, 5))))
    }
    "Support during" in {
      Subst(3, 4, 0, 4).carved(Subst(1, 3, 1, 5) :: Nil) must_==
        (List(Subst(3, 4, 3 + 2, 4 + 2), Subst(3, 4, 0, 1)), List(Subst(Interval(1, 3), Interval(1, 5))))
    }
    "Support finishesOrIOverlaps" in {
      Subst(3, 4, 0, 2).carved(Subst(1, 3, 1, 5) :: Nil) must_==
        (List(Subst(3, 4, 0, 1)), List(Subst(Interval(1, 3), Interval(1, 5))))
    }
    "Support IbeforeOrMeets" in {
      Subst(3, 4, 0, 1).carved(Subst(1, 3, 1, 5) :: Nil) must_==
        (List(Subst(3, 4, 0, 1)), List(Subst(Interval(1, 3), Interval(1, 5))))
    }
    "Support else" in {
      val nothing = (Nil, List(Subst(Interval(1, 4), Interval(1, 5))))
      Subst(3, 4, 1, 4).carved(Subst(1, 4, 1, 5) :: Nil) must_== nothing
      Subst(3, 4, 1, 3).carved(Subst(1, 4, 1, 5) :: Nil) must_== nothing
      Subst(3, 4, 2, 4).carved(Subst(1, 4, 1, 5) :: Nil) must_== nothing
      Subst(3, 4, 2, 3).carved(Subst(1, 4, 1, 5) :: Nil) must_== nothing
    }

  }

  "TrackString: Repl#carve(List[Subst])" should {
    "Support beforeOrMeets" in {
      Repl(3, 4, 4, 5).carve(Subst(1, 3, 1, 5) :: Nil) must_== Subst(3, 4, 4 + 2, 5 + 2) :: Nil
    }
    "Support overlapsOrStarts" in {
      Repl(3, 4, 2, 4).carve(Subst(1, 3, 1, 5) :: Nil) must_== Subst(3, 4, 3 + 2, 4 + 2) :: Nil
    }
    "Support during" in {
      Repl(3, 4, 0, 4).carve(Subst(1, 3, 1, 5) :: Nil) must_== Subst(3, 4, 3 + 2, 4 + 2) :: Subst(3, 4, 0, 1) :: Nil
    }
    "Support finishesOrIOverlaps" in {
      Repl(3, 4, 0, 2).carve(Subst(1, 3, 1, 5) :: Nil) must_== Subst(3, 4, 0, 1) :: Nil
    }
    "Support IbeforeOrMeets" in {
      Repl(3, 4, 0, 1).carve(Subst(1, 3, 1, 5) :: Nil) must_== Subst(3, 4, 0, 1) :: Nil
    }
    "Support other cases" in {
      Repl(3, 4, 1, 4).carve(Subst(1, 4, 1, 5) :: Nil) must_== Nil
      Repl(3, 4, 1, 3).carve(Subst(1, 4, 1, 5) :: Nil) must_== Nil
      Repl(3, 4, 2, 4).carve(Subst(1, 4, 1, 5) :: Nil) must_== Nil
      Repl(3, 4, 2, 3).carve(Subst(1, 4, 1, 5) :: Nil) must_== Nil
    }
  }

  "TrackString: Repl#srcPos" should {
    "Support beforeOrMeets" in {
      Repl(1, 2, 1, 4).srcPos(4, 6) must_== Interval(4 - 2, 6 - 2)
    }
    "Support overlapsOrStarts" in {
      Repl(1, 2, 3, 4).srcPos(3, 6) must_== Interval(3 - 2, 6 - 2)
    }
    "Support during" in {
      Repl(1, 2, 3, 4).srcPos(0, 6) must_== Interval(0, 6 - 2)
    }
    "Support finishesOrIOverlaps" in {
      Repl(1, 2, 3, 4).srcPos(5, 6) must_== Interval(5 - 2, 6 - 2)
    }
    "Support IbeforeOrMeets" in {
      Repl(1, 2, 3, 4).srcPos(5, 6) must_== Interval(5 - 2, 6 - 2)
    }
    "Support other cases" in {
      Repl(1, 2, 3, 4).srcPos(5, 6) must_== Interval(5 - 2, 6 - 2)
    }
  }

  "TrackString: update" should {
    "Support beforeOrMeets" in {
      Repl(1, 2, 3, 4).update(Subst(5, 6, 7, 8) :: Nil) must_== Subst(5 - 2, 6 - 2, 7, 8) :: Nil
    }
    "Support overlapsOrStarts" in {
      Repl(1, 2, 3, 4).update(Subst(3, 6, 7, 8) :: Nil) must_== Subst(3 - 2, 6 - 2, 7, 8) :: Nil
    }
    "Support during" in {
      Repl(1, 2, 3, 4).update(Subst(0, 6, 7, 8) :: Nil) must_== Subst(0, 6 - 2, 7, 8) :: Nil
    }
    "Support finishesOrIOverlaps" in {
      Repl(1, 2, 3, 4).update(Subst(5, 6, 7, 8) :: Nil) must_== Subst(5 - 2, 6 - 2, 7, 8) :: Nil
    }
    "Support IbeforeOrMeets" in {
      Repl(1, 2, 3, 4).update(Subst(5, 6, 7, 8) :: Nil) must_== Subst(5 - 2, 6 - 2, 7, 8) :: Nil
    }
    "Support other cases" in {
      Repl(1, 2, 3, 4).update(Subst(5, 6, 7, 8) :: Nil) must_== Subst(5 - 2, 6 - 2, 7, 8) :: Nil
    }
  }

  "TrackString: edit" should {
    "Support deletion aBc => ac" in {
      val res = TrackString("aBc", "ac", Repl(1, 2, 1, 1))
      res.repl must_== Repl(1, 2, 1, 1)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(2, 3)
      res.srcPos(0, 2) must_== Interval(0, 3)
    }
    "Support multiple deletion aBcDe => ace" in {
      val res = TrackString("aBcDe")
        .edit("acDe", Repl(1, 2, 1, 1))
        .edit("ace", Repl(2, 3, 2, 2))
      res.repl must_== Repl(1, 2, 1, 1) + (3, 4, 2, 2)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(2, 3)
      res.srcPos(2, 3) must_== Interval(4, 5)
    }
    "Support insertion ac => aBc" in {
      val res = TrackString("ac", "aBc", Repl(1, 1, 1, 2))
      res.repl must_== Repl(1, 1, 1, 2)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 1)
      res.srcPos(2, 3) must_== Interval(1, 2)
      res.srcPos(0, 3) must_== Interval(0, 2)
    }
    "Support multiple insertion ace => aBcDe" in {
      val res = TrackString("ace")
        .edit("acDe", Repl(2, 2, 2, 3))
        .edit("aBcDe", Repl(1, 1, 1, 2))
      res.repl must_== Repl(1, 1, 1, 2) + (2, 2, 3, 4)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 1)
      res.srcPos(2, 3) must_== Interval(1, 2)
      res.srcPos(3, 4) must_== Interval(2, 2)
      res.srcPos(4, 5) must_== Interval(2, 3)
      res.srcPos(0, 5) must_== Interval(0, 3)
    }
    "Support same-length replacement abcd => aBCd" in {
      val res = TrackString("abcd", "aBCd", Repl(1, 3, 1, 3))
      res.repl must_== Repl(1, 3, 1, 3)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 3)
      res.srcPos(2, 3) must_== Interval(1, 3)
      res.srcPos(3, 4) must_== Interval(3, 4)
      res.srcPos(0, 4) must_== Interval(0, 4)
    }
    "Support multiple same-length replacement abcd => aBcd" in {
      val res = TrackString("abcd")
        .edit("aBcd", Repl(1, 2, 1, 2))
        .edit("aBcD", Repl(3, 4, 3, 4))
      res.repl must_== Repl(1, 2, 1, 2) + (3, 4, 3, 4)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 2)
      res.srcPos(2, 3) must_== Interval(2, 3)
      res.srcPos(3, 4) must_== Interval(3, 4)
      res.srcPos(0, 4) must_== Interval(0, 4)
    }
    "Support intertwined same-length replacement abcd => aBCd => aBED" in {
      val res = TrackString("abcd")
        .edit("aBCd", Repl(1, 3, 1, 3))
        .edit("aBED", Repl(2, 4, 2, 4))
      // First Subst pieces are merged
      res.repl must_== Repl(Subst(1, 4, 2, 4) :: Subst(1, 3, 1, 2) :: Nil)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 3)
      res.srcPos(2, 3) must_== Interval(1, 4)
      res.srcPos(3, 4) must_== Interval(1, 4)
      res.srcPos(0, 4) must_== Interval(0, 4)
    }
    "Support compression abcd => aEd" in {
      val res = TrackString("abcd", "aEd", Repl(1, 3, 1, 2))
      res.repl must_== Repl(1, 3, 1, 2)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 3)
      res.srcPos(2, 3) must_== Interval(3, 4)
      res.srcPos(0, 3) must_== Interval(0, 4)
    }
    "Support expansion aed => aBCd" in {
      val res = TrackString("aed", "aBCd", Repl(1, 2, 1, 3))
      res.repl must_== Repl(1, 2, 1, 3)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 2)
      res.srcPos(2, 3) must_== Interval(1, 2)
      res.srcPos(3, 4) must_== Interval(2, 3)
      res.srcPos(0, 4) must_== Interval(0, 3)
    }
    "Support multiple compression abcd => aEd => aF" in {
      val res = TrackString("abcd")
        .edit("aEd", Repl(1, 3, 1, 2))
        .edit("aF", Repl(1, 3, 1, 2))
      res.repl must_== Repl(1, 4, 1, 2)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 4)
      res.srcPos(0, 2) must_== Interval(0, 4)
    }
    "Support multiple expansion af => aED => aBCd" in {
      val res = TrackString("aF")
        .edit("aED", Repl(1, 2, 1, 3))
        .edit("aBCD", Repl(1, 2, 1, 3))
      res.repl must_== Repl(1, 2, 1, 4)
      res.srcPos(0, 1) must_== Interval(0, 1)
      res.srcPos(1, 2) must_== Interval(1, 2)
      res.srcPos(2, 3) must_== Interval(1, 2)
      res.srcPos(3, 4) must_== Interval(1, 2)
      res.srcPos(0, 4) must_== Interval(0, 2)
    }

  }

  "TrackString: replaceAll " should {
    "Support no-op aBc => aBc" in {
      val as = TrackString("aBc")
      val res = as.replaceAll("B".r, "B")
      res.toString.toString must_== "aBc"
      res.repl.substs must be empty
    }
    "Support deletion aBc => ac" in {
      val as = TrackString("aBc")
      val res = as.replaceAll("B".r, "")
      res.toString.toString must_== "ac"
    }
    "Support multiple replacements aBCDef => axef => axhijf => axhjf" in {
      val as = TrackString("aBCDef")

      val res1 = as.replaceAll("BCD".r, "x")
      res1.toString.toString must_== "axef"
      res1.repl must_== Repl(1, 4, 1, 2)

      val res2 = res1.replaceAll("e".r, "hij")
      res2.toString.toString must_== "axhijf"
      res2.repl must_== Repl(List(Subst(4, 5, 2, 5), Subst(1, 4, 1, 2)))

      val res = res2.replaceAll("i".r, "")
      res.toString.toString must_== "axhjf"
      res.repl must_== Repl(List(Subst(4, 5, 2, 4),
                                 Subst(1, 4, 1, 2)))
      res.srcPos(0, 1) must_== Interval(0, 1) // a
      res.srcPos(1, 2) must_== Interval(1, 4) // x comes from BCD
      res.srcPos(2, 3) must_== Interval(4, 5) // h comes from e
      res.srcPos(3, 4) must_== Interval(4, 5) // j too
      res.srcPos(4, 5) must_== Interval(5, 6) // f
    }
  }

  "TrackString: replaceAll (single)" should {
    "replaceAll with groups accessed by number" in {
      TrackString("abcd").replaceAll("(bc)".r, "$1$1").toString must_== "abcbcd"
    }
    "replaceAll with scala-named groups accessed by name" in {
      val result = TrackString("abcd").replaceAll(new Regex("(bc)", "gn"), "${gn}${gn}").toString must_== "abcbcd"
    }
    "not support replaceAll with java-named groups accessed by name" in {
      if (TrackString.supportsNamedGroup) {
        TrackString("abcd").replaceAll("(?<gn>bc)".r, "${gn}${gn}") must throwAn[java.lang.IllegalArgumentException]
        // ._1 must_== "abcbcd"
      } else {
        TrackString("abcd").replaceAll("(?<gn>bc)".r, "${gn}${gn}") must throwA[java.util.regex.PatternSyntaxException]
      }
    }
    "replaceAll with custom replacer function" in {
      def replacer(init: String): (String, Repl) = {
        val result = new StringBuilder
        var repl = new Repl
        for ((char, i) <- init.zipWithIndex) {
          val newChar =
            if (!char.isLetter)    ""
            else if (char.isUpper) char.toLower.toString
            else                   char.toUpper.toString * 2

          result.append(newChar)
          if (newChar.length != 1) {
            repl = repl.+(i, 1, newChar.length)
          }
        }
        (result.toString, repl)
      }
      val as = TrackString("0a1B2c3D4e5")
        .edit(replacer _)
      as.toString must_== "AAbCCdEE"
      as.srcPos(0,1) must_== Interval(1,2)
      as.srcPos(0,2) must_== Interval(1,2)
      as.srcPos(0,3) must_== Interval(1,4)
      as.srcPos(0,3) must_== Interval(1,4)
      as.srcPos(1,2) must_== Interval(1,2)
      as.srcPos(1,3) must_== Interval(1,4)
      as.srcPos(2,3) must_== Interval(3,4)
      as.srcPos(2,4) must_== Interval(3,6)
      as.srcPos(6,7) must_== Interval(9,10)
      as.srcPos(6,8) must_== Interval(9,10)
      as.srcPos(7,8) must_== Interval(9,10)
    }
    "replaceAll with custom per-char replacer function" in {
      def replacer(c: Char): Option[String] =
        if (c.isDigit) Some("")
        else if (c.isUpper) Some(c.toString.toLowerCase)
        else if (c.isLower) Some(c.toString.toUpperCase * 2)
        else None

      var ts = TrackString(" 0a1B2c3D4e5.").replaceAll(replacer _)
      ts.toString must_== " AAbCCdEE."
      ts.srcPos(0)    must_== Interval(0, 1)
      ts.srcPos(1)    must_== Interval(2, 3)
      ts.srcPos(2)    must_== Interval(2, 3)
      ts.srcPos(1, 2) must_== Interval(2, 3)
      ts.srcPos(3)    must_== Interval(4, 5)
      ts.srcPos(4)    must_== Interval(6, 7)
      ts.srcPos(5)    must_== Interval(6, 7)
      ts.srcPos(6)    must_== Interval(8, 9)
      ts.srcPos(7)    must_== Interval(10, 11)
      ts.srcPos(8)    must_== Interval(10, 11)
      ts.srcPos(9)    must_== Interval(12, 13)

      val charMap = Map('œ' -> "oe")
      ts = TrackString("œil pour œil").replaceAll(charMap.get _)
      ts.toString  must_== "oeil pour oeil"
      ts.repl      must_== Repl(Subst(9, 10, 10, 12) :: Subst(0, 1, 0, 2) :: Nil)
      ts.srcPos(0) must_== Interval(0, 1)
      ts.srcPos(1) must_== Interval(0, 1)
      ts.srcPos(2) must_== Interval(1, 2)
    }
  }

  "TrackString: replaceAll (multiple)" should {
    "still perform replacements" in {
      TrackString("aBcDe").replaceAll(("B".r, "b"), ("D".r, "d")).toString must_== "abcde"
    }
  }

  "TrackString: replaceFirst " should {
    "replaceFirst Regex with CharSequence" in {
      TrackString("aBBcDDe")
        .replaceFirst("B".r, "b")
        .replaceFirst("D".r, "d")
        .toString must_== "abBcdDe"
    }
    "replaceFirst Regex with replacer method" in {
      TrackString("aBBcDDe")
        .replaceFirst("B".r, TrackString.defaultReplacer("b"))
        .replaceFirst("D".r, TrackString.defaultReplacer("d"))
        .toString must_== "abBcdDe"
    }
  }

  "TrackString: regexReplacer / defaultReplacer" should {
    import TrackString.defaultReplacer
    import java.lang.{IllegalArgumentException => IAE}

    val a = TrackString("a")
    val xaac = TrackString("xaaaaaaaac")

    "support Java regex specs" in {
      def test(subject: String, regex: Regex, replace: CharSequence, expected: Either[Boolean, String]) = {
        val m = regex.findFirstMatchIn(subject).get
        expected match {
          case Left(false) =>       // expected to fail on template compilation
            TrackString.regexReplacer(replace) must throwAn[IAE]
          case Left(true) =>        // expected to fail when applied to Match
            val replacer = TrackString.regexReplacer(replace)
            replacer(m) must throwAn[IAE]
          case Right(str) =>        // expected to succeed and provide the contained String
            val replacer = TrackString.regexReplacer(replace)
            replacer(m) must_== str
        }
      }

      test("abc", "b".r,    "B",       Right("B"))    // raw literal
      test("abc", "b".r, "\\B\\$\\\\", Right("B$\\")) // escaped literal
      test("abc", "b".r,    "\\B\\",   Left(false))   // unexpected ending  => fail compilation
      test("abc", "b".r,    "$",       Left(false))   // incorrect group    => fail compilation
      test("abc", "b".r,    "$a",      Left(false))   // incorrect group    => fail compilation
      test("abc", "b".r,    "$1",      Left(true))    // unknown group num  => fail resolution
      test("abc", "bc".r,   "$0",      Right("bc"))   // whole match
      test("abc", "(b)c".r, "$1",      Right("b"))    // numbered group
      test("abc", "(b)c".r, "$10",     Right("b0"))   // numbered group (with bigger number)
      test("abc", "(b)c".r, "B$10\\B", Right("Bb0B")) // all at once

      TrackString.regexReplacer("${g}", false) must throwAn[IAE] // group name not used  => fail compilation
      TrackString.regexReplacer("${",    true) must throwAn[IAE] // incorrect group      => fail compilation
      TrackString.regexReplacer("${g",   true) must throwAn[IAE] // incorrect group      => fail compilation
      TrackString.regexReplacer("${}",   true) must throwAn[IAE] // incorrect group name => fail compilation
      TrackString.regexReplacer("${1a}", true) must throwAn[IAE] // incorrect group name => fail compilation
      TrackString.regexReplacer("${-a}", true) must throwAn[IAE] // incorrect group name => fail compilation
      TrackString.regexReplacer("${a-}", true) must throwAn[IAE] // incorrect group name => fail compilation

      val re = new Regex("(b)c", "g")
      val m = re.findFirstMatchIn("abc").get
      TrackString.regexReplacer("${g}", true)(m) must_== "b"        // known group name
      TrackString.regexReplacer("${g2}", true)(m) must throwAn[IAE] // unknown group name => fail resolution
    }

    "be used by replaceAll" in {
      a.replaceAll("(a)".r, "$1b$1").toString  must_== "aba"
      a.replaceAll("(a)".r, "$1b$10").toString  must_== "aba0"
      a.replaceAll("(a)".r, "$1b$9").toString must throwAn[java.lang.IllegalArgumentException]
      xaac.replaceAll("(x)(a)(a)(a)(a)(a)(a)(a)(a)(c)".r, "$1b$10").toString  must_== "xbc"
      a.replaceAll(new Regex("(a)", "g1"), "${g1}b${g1}").toString  must_== "aba"
      a.replaceAll(new Regex("(a)", "g1"), "$1b$1").toString  must_== "aba"
      a.replaceAll("(?<g1>a)".r, "$1b$1").toString  must_== "aba"
      a.replaceAll("(?<g1>a)".r, "${g1}b${g1}") must throwAn[java.lang.IllegalArgumentException]
    }

    "stay correct when useNamedGroup = false" in {
      a.replaceAll("(a)".r, defaultReplacer("$1b$1", false)).toString must_== "aba"
      a.replaceAll("(a)".r, defaultReplacer("$1b$10", false)).toString must_== "aba0"
      a.replaceAll("(a)".r, defaultReplacer("$1b$9", false)).toString must throwAn[java.lang.IllegalArgumentException]
      xaac.replaceAll("(x)(a)(a)(a)(a)(a)(a)(a)(a)(c)".r, defaultReplacer("$1b$10", false)).toString must_== "xbc"
      a.replaceAll(new Regex("(a)", "g1"), defaultReplacer("${g1}b${g1}", false)) must throwAn[java.lang.IllegalArgumentException]
      a.replaceAll(new Regex("(a)", "g1"), defaultReplacer("$1b$1", false)).toString must_== "aba"
      a.replaceAll(new Regex("(?<g1>a)", ""), defaultReplacer("$1b$1", false)).toString must_== "aba"
      a.replaceAll(new Regex("(?<g1>a)", ""), defaultReplacer("${g1}b${g1}", false)) must throwAn[java.lang.IllegalArgumentException]
    }

  }

}