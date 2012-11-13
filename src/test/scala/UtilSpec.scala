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

}