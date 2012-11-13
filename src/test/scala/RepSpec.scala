package fr.splayce.REL

import org.specs2.mutable._

class RepSpec extends Specification {

  import Symbols._
  import scala.util.matching.Regex
  import Implicits.RE2Regex

  "Repeater short syntax" should {
    val a = RE("a")
    import Implicits._

    "expr {3}   --> {3}"     in { (a{3})    === RepExactlyN(a.ncg, 3) }
    "expr >  3  --> {3,}"    in { (a >  3)  === RepAtLeastN(a.ncg, 3,    Greedy) }
    "expr >? 3  --> {3,}?"   in { (a >? 3)  === RepAtLeastN(a.ncg, 3,    Reluctant) }
    "expr >+ 3  --> {3,}+"   in { (a >+ 3)  === RepAtLeastN(a.ncg, 3,    Possessive) }
    "expr <  3  --> {0,3}"   in { (a <  3)  ===  RepAtMostN(a.ncg, 3,    Greedy) }
    "expr.<?(3) --> {0,3}?"  in { (a.<?(3)) ===  RepAtMostN(a.ncg, 3,    Reluctant) }
    "expr <+ 3  --> {0,3}+"  in { (a <+ 3)  ===  RepAtMostN(a.ncg, 3,    Possessive) }
    "expr {1->3}  --> {1,3}" in { (a{1->3})   ===   RepNToM(a.ncg, 1, 3, Greedy) }
    "expr (1,3)   --> {1,3}" in { (a(1,3))    ===   RepNToM(a.ncg, 1, 3, Greedy) }
    "expr {1 to3} --> {1,3}" in { (a{1 to 3}) ===   RepNToM(a.ncg, 1, 3, Greedy) }
    "expr(1, 3, Reluctant)  --> {1,3}?" in { (a(1, 3, Reluctant))  === RepNToM(a.ncg, 1, 3, Reluctant) }
    "expr(1, 3, Possessive) --> {1,3}+" in { (a(1, 3, Possessive)) === RepNToM(a.ncg, 1, 3, Possessive) }
  }

  "Opt greedy repeater" should {
    val alphas = α ?

    "linearize to a question mark" in {
      alphas.toString must endWith("?")
    }
    "match zero occurence" in {
      "" must be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "not match multiple occurences" in {
      "abc" must not be matching(alphas)
    }
    "be greedy" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "a"
      first must_== "a"
      second must be empty
    }
  }

  "Opt reluctant repeater" should {
    val alphas = α ??

    "linearize to two question marks" in {
      alphas.toString must endWith("??")
    }
    "match zero occurence" in {
      "" must be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "not match multiple occurences" in {
      "abc" must not be matching(alphas)
    }
    "be reluctant" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "a"
      first must be empty ;
      second must_== "a"
    }
  }

  "Opt possessive repeater" should {
    val alphas = α ?+

    "linearize to a question mark followed by a Kleene Cross" in {
      alphas.toString must endWith("?+")
    }
    "match zero occurence" in {
      "" must be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "not match multiple occurences" in {
      "abc" must not be matching(alphas)
    }
    "be possessive" in {
      val a = Atom("a".r)
      "a" must not be matching((α ?+) ~ a)
      // whereas
      "a" must be matching((α ?) ~ a)
    }
  }

  "Kleene Star greedy repeater" should {
    val alphas = α *

    "linearize to a Kleene Star" in {
      alphas.toString must endWith("*")
    }
    "match zero occurence" in {
      "" must be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "match multiple occurences" in {
      "abc" must be matching(alphas)
    }
    "be greedy" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "abc"
      first must_== "abc"
      second must be empty
    }
  }

  "Kleene Star reluctant repeater" should {
    val alphas = α *?

    "linearize to a Kleene Star followed by a question mark" in {
      alphas.toString must endWith("*?")
    }
    "match zero occurence" in {
      "" must be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "match multiple occurences" in {
      "abc" must be matching(alphas)
    }
    "be reluctant" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "abc"
      first must be empty ;
      second must_== "abc"
    }
  }

  "Kleene Star possessive repeater" should {
    val alphas = α *+

    "linearize to a Kleene Star followed by a Kleene Cross" in {
      alphas.toString must endWith("*+")
    }
    "match zero occurence" in {
      "" must be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "match multiple occurences" in {
      "abc" must be matching(alphas)
    }
    "be possessive" in {
      val a = Atom("a".r)
      "a" must not be matching((α *+) ~ a)
      // whereas
      "a" must be matching((α *) ~ a)
    }
  }

  "Kleene Cross greedy repeater" should {
    val alphas = α +

    "linearize to a Kleene Cross" in {
      alphas.toString must endWith("+")
    }
    "not match zero occurence" in {
      "" must not be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "match multiple occurences" in {
      "abc" must be matching(alphas)
    }
    "be greedy" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "abc"
      first must_== "ab"
      second must_== "c"
    }
  }

  "Kleene Cross reluctant repeater" should {
    val alphas = α +?

    "linearize to a Kleene Cross followed by a question mark" in {
      alphas.toString must endWith("+?")
    }
    "not match zero occurence" in {
      "" must not be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "match multiple occurences" in {
      "abc" must be matching(alphas)
    }
    "be reluctant" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "abc"
      first must_== "a"
      second must_== "bc"
    }
  }

  "Kleene Cross possessive repeater" should {
    val alphas = α ++

    "linearize to a Kleene Cross followed by a Kleene Cross" in {
      alphas.toString must endWith("++")
    }
    "not match zero occurence" in {
      "" must not be matching(alphas)
    }
    "match one occurence" in {
      "a" must be matching(alphas)
    }
    "match multiple occurences" in {
      "abc" must be matching(alphas)
    }
    "be possessive" in {
      val a = Atom("a".r)
      "aa" must not be matching((α ++) ~ a)
      // whereas
      "aa" must be matching((α +) ~ a)
    }
  }

  "N times repeater (testing with 2)" should {
    val alphas = α{2}

    "linearize to {N}" in {
      alphas.toString must endWith("{2}")
    }
    "not match zero occurence" in {
      "" must not be matching(alphas)
    }
    "not match one occurence" in {
      "a" must not be matching(alphas)
    }
    "match N occurences" in {
      "ab" must be matching(alphas)
    }
    "not match N+1 occurences" in {
      "abc" must not be matching(alphas)
    }
  }

  "N-to-M times greedy repeater (testing with {2,3})" should {
    val alphas = α(2 to 3)

    "linearize to {N,M}" in {
      alphas.toString must endWith("{2,3}")
    }
    "not match zero occurence" in {
      "" must not be matching(alphas)
    }
    "not match one occurence" in {
      "a" must not be matching(alphas)
    }
    "match N occurences" in {
      "ab" must be matching(alphas)
    }
    "match M occurences" in {
      "abc" must be matching(alphas)
    }
    "not match M+1 occurences" in {
      "abcd" must not be matching(alphas)
    }
    "be greedy" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "abcde"
      first must_== "abc"
      second must_== "de"
    }
  }

  "N-or-more times greedy repeater (testing with {2,})" should {
    val alphas = α > 2

    "linearize to {N,}" in {
      alphas.toString must endWith("{2,}")
    }
    "not match zero occurence" in {
      "" must not be matching(alphas)
    }
    "not match one occurence" in {
      "a" must not be matching(alphas)
    }
    "match N occurences" in {
      "ab" must be matching(alphas)
    }
    "match more than N occurences" in {
      "abc" must be matching(alphas)
      "abcdefghijklmnopqrstuvwxyz" must be matching(alphas)
    }
    "be greedy" in {
      val Twice :Regex = alphas.g ~ alphas.g
      val Twice(first, second) = "abcde"
      first must_== "abc"
      second must_== "de"
    }
  }

}