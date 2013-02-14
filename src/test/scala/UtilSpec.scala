package fr.splayce.rel.util

import org.specs2.mutable._

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.rel._
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

  "Pattern matching utils on extractors" should {
    val nfDateX = matchers.DateExtractor.NUMERIC_FULL
    val nfDate  = matchers.Date.NUMERIC_FULL

    "allow pattern match on a String" in {
      ("21/10/2000" match {
        case nfDateX(List(d)) => d.toString == "Y2000 Y00 M10 D21"
        case _ => false
      }).must(beTrue)
    }

    "allow pattern match on a Match" in {
      val nfd = nfDateX.toPM
      val m = nfDate.r.findFirstMatchIn("21/10/2000").get
      (m match {
        case nfd(List(d)) => d.toString == "Y2000 Y00 M10 D21"
        case _ => false
      }).must(beTrue)
    }

    "allow pattern matching with MatchGroup" in {
      val m = nfDate.r.findFirstMatchIn("21/10/2000").get
      (m match {
        case nfDate.matchGroup(d) => d.name == Some("n_f") && d.matched == Some("21/10/2000")
        case _ => false
      }).must(beTrue)
    }
  }

  "Extractor" should {
    import ByOptionExtractor._

    val re = ((("." \ "a") - (".".? \ "b") - ("." \ "c").?) \ "abc") - (".".? \ "d")
    val str = "abcdabc"
    (re.r findAllIn str).matchData.toList map { m => re.matchGroup(m) } match {
      case MatchGroup(None, Some("abcd"), List(
            MatchGroup(Some("abc"), Some("abc"), List(
              MatchGroup(Some("a"), Some("a"), Nil),
              MatchGroup(Some("b"), Some("b"), Nil),
              MatchGroup(Some("c"), Some("c"), Nil))),
            MatchGroup(Some("d"), Some("d"), Nil)))
        :: MatchGroup(None, Some("abc"), List(
            MatchGroup(Some("abc"), Some("abc"), List(
              MatchGroup(Some("a"), Some("a"), Nil),
              MatchGroup(Some("b"), Some("b"), Nil),
              MatchGroup(Some("c"), Some("c"), Nil))),
            MatchGroup(Some("d"), Some(""), Nil)))
        :: Nil => // ok
      case mg =>
        println(mg mkString "\n")
        throw new RuntimeException("Test precondition failed")
    }

    "be obtainable from (Match => Option[A])" in {
      val extract = re << { (m: Match) => Some(m.group("b")) }
      extract(str).toList must_== List("b", "b")
    }
    "be obtainable from Partial(Match => A)" in {
      val extr: MatchExtractor[String] = {
        case m if (m.group("c") != null) => m.group("b") }
      val extract = re << extr
      extract(str).toList must_== List("b", "b")
    }
    "be obtainable from Partial(MatchGroup => A)" in {
      val extr: MatchGroupExtractor[String] = {
        case MatchGroup(_, Some(_), List(MatchGroup(Some("abc"), Some(abc), _), _*)) => abc }
      val extract = re << lift(extr)
      extract(str).toList must_== List("abc", "abc")
    }
    "be obtainable from Partial(MatchGroup => Option[A])" in {
      val extr: MatchGroupOptionExtractor[String] = {
        case MatchGroup(_, Some(_), List(MatchGroup(Some("abc"), abc, _), _*)) => abc }
      val extract = re << lift(extr)
      extract(str).toList must_== List("abc", "abc")
    }

    "be obtainable from Partial(MatchGroup => A) with direct dive into first non-empty group" in {
      val extr: MatchGroupExtractor[String] = {
        case MatchGroup(Some("abc"), Some(abc), _) => abc }
      val extract = re << firstNES(lift(extr))
      extract(str).toList must_== List("abc", "abc")
    }
    "be obtainable from Partial(MatchGroup => Option[A]) with direct dive into first non-empty group" in {
      val extr: MatchGroupOptionExtractor[String] = {
        case MatchGroup(Some("abc"), abc, _) => abc }
      val extract = re << firstNES(lift(extr))
      extract(str).toList must_== List("abc", "abc")
    }

    "allow pattern matching on multiple instance of a named group" in {
      import matchers.{Date, DateExtractor}
      import DateExtractor.{Result => RDate}
      type DoubleDate = (RDate, RDate)

      val nfDatePattern = toPM(DateExtractor.Numeric)
      val nfDate  = Date.NUMERIC_FULL
      val re = (nfDate \ "date") - " TO " - (nfDate \ "date")

      val extr: MatchGroupOptionExtractor[DoubleDate] = {
        case MatchGroup(None, Some(_),
              MatchGroup(Some("date"), Some(_), List(nfDatePattern(d1)))
          ::  MatchGroup(Some("date"), Some(_), List(nfDatePattern(d2)))
          :: _) => Some((d1.head, d2.head))
      }

      val extract = re << extr
      ("21/10/2000 TO 31/10/2000" match {
        case extract((d1, d2)) =>
          (d1.toString == "Y2000 Y00 M10 D21") && (d2.toString == "Y2000 Y00 M10 D31")
        case _ => false
      }).must(beTrue)
    }
  }


  "Group names regex" should {
    "match strict (Java 7 compatible) group names (1 alpha + 0..* alphanum)" in {
      "group"   must     be matching(RE.strictGroupName)
      "group1"  must     be matching(RE.strictGroupName)
      "group_1" must not be matching(RE.strictGroupName)
      "group-1" must not be matching(RE.strictGroupName)
      "group*1" must not be matching(RE.strictGroupName)
      "group 1" must not be matching(RE.strictGroupName)
      "group>1" must not be matching(RE.strictGroupName)
      "1group"  must not be matching(RE.strictGroupName)
    }
    "match snake (.NET/PCRE compatible) group names (1 alpha or '_' + 0..* alphanum or '_')" in {
      "group"   must     be matching(RE.snakeGroupName)
      "group1"  must     be matching(RE.snakeGroupName)
      "group_1" must     be matching(RE.snakeGroupName)
      "group*1" must not be matching(RE.snakeGroupName)
      "group 1" must not be matching(RE.snakeGroupName)
      "group-1" must not be matching(RE.snakeGroupName)
      "group>1" must not be matching(RE.snakeGroupName)
      "1group"  must not be matching(RE.snakeGroupName)
    }
    "match strict (Ruby 1.9+ compatible) group names (1 alpha or '_' + 0..* ASCII printable but '>')" in {
      "group"   must     be matching(RE.lenientGroupName)
      "group1"  must     be matching(RE.lenientGroupName)
      "group_1" must     be matching(RE.lenientGroupName)
      "group-1" must     be matching(RE.lenientGroupName)
      "group*1" must     be matching(RE.lenientGroupName)
      "group 1" must     be matching(RE.lenientGroupName)
      "group>1" must not be matching(RE.lenientGroupName)
      "1group"  must not be matching(RE.lenientGroupName)
    }
  }

}