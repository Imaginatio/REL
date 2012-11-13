package fr.splayce.REL.util

import org.specs2.mutable._

import fr.splayce.REL._
import Implicits.{RE2Regex, string2RE}


class UtilSpec extends Specification {

  "Matched Extractor" should {
    "return all matches" in {
      val extract = RE("a") << MatchedExtractor()
      extract("aaa").toList must_== List("a", "a", "a")
    }
  }

  "Nth Group Extractor" should {
    "by default, return first group for all matches" in {
      val extract = ("a" g) << NthGroupExtractor()
      extract("aaa").toList must_== List("a", "a", "a")
    }
    "return nth group for all matches" in {
      val extract = ("a".g - "b".g) << NthGroupExtractor(2)
      extract("ababab").toList must_== List("b", "b", "b")
    }
    "return empty for match with not enough groups" in {
      val extract = ("a".g - "b".g) << NthGroupExtractor(3)
      extract("ababab").toList must_== Nil
    }
  }

  "Named Group Extractor" should {
    "return named group" in {
      val extract = (("a" \ "g") - "b".g) << NamedGroupExtractor("g")
      extract("ababab").toList must_== List("a", "a", "a")
    }
    "return empty for match with not enough groups" in {
      val extract = (("a" \ "g") - "b".g) << NamedGroupExtractor("x")
      extract("ababab").toList must_== Nil
    }
  }


  "Pattern matching utils" should {
    val nfDate = matchers.DateExtractor.NUMERIC_FULL
    "allow pattern match on a String" in {
      ("21/10/2000" match {
        case nfDate(List(d)) => d.toString == "Y2000 Y00 M10 D21"
        case _ => false
      }).must(beTrue)
    }
    "allow pattern match on a Match" in {
      import nfDate.{matchPattern => nfd}
      val m = nfDate.regex.findFirstMatchIn("21/10/2000").get
      (m match {
        case nfd(List(d)) => d.toString == "Y2000 Y00 M10 D21"
        case _ => false
      }).must(beTrue)
    }
  }


  "MatchGroups" should {
    import MatchGroups._

    type MES = MatchExtractor[String]
    val re = ("." \ "a") - (".".? \ "b") - ("." \ "c").?

    "allow pattern matching on captured groups" in {
      val pf: MES = {
        case MatchGroups(_, _,  c)    if (c != null) => c
        case MatchGroups(_, b,  null) if (b != "")   => b
        case MatchGroups(a, "", null)                => a
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }

    "allow pattern matching on Option(captured groups) (Some = not null)" in {
      val pf: MES = {
        case NotNull(_,       _,       Some(c))           => c
        case NotNull(_,       Some(b), None) if (b != "") => b
        case NotNull(Some(a), _, _)                       => a
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }
    "allow pattern matching on Map(name, Option(captured groups)) (Some = not null)" in {
      val pf: MES = {
        case NotNull.NamedMap(m) if (m("c").isDefined) => m("c").get
        case NotNull.NamedMap(m) if (m("b").get != "") => m("b").get
        case NotNull.NamedMap(m)                       => m("a").get
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }
    "allow pattern matching on pair List((name, Option(captured groups))) (Some = not null)" in {
      val pf: MES = {
        case NotNull.NamedPairs(_, _, ("c", Some(c)))              => c
        case NotNull.NamedPairs(_, ("b", Some(b)), _) if (b != "") => b
        case NotNull.NamedPairs(("a", Some(a)), _, _)              => a
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }

    "allow pattern matching on Option(captured groups) (Some = not empty)" in {
      val pf: MES = {
        case NotEmpty(_,       _,       Some(c)) => c
        case NotEmpty(_,       Some(b), None)    => b
        case NotEmpty(Some(a), _,       _)       => a
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }
    "allow pattern matching on Map(name, Option(captured groups)) (Some = not empty)" in {
      val pf: MES = {
        case NotEmpty.NamedMap(m) if (m("c").isDefined) => m("c").get
        case NotEmpty.NamedMap(m) if (m("b").isDefined) => m("b").get
        case NotEmpty.NamedMap(m)                       => m("a").get
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }
    "allow pattern matching on pair List((name, Option(captured groups))) (Some = not empty)" in {
      val pf: MES = {
        case NotEmpty.NamedPairs(_, _, ("c", Some(c))) => c
        case NotEmpty.NamedPairs(_, ("b", Some(b)), _) => b
        case NotEmpty.NamedPairs(("a", Some(a)), _, _) => a
      }
      val extract = re << pf
      extract("123") .toList must_== List("3")
      extract("12")  .toList must_== List("2")
      extract("1")   .toList must_== List("1")
      extract("1234").toList must_== List("3", "4")
    }
  }

}