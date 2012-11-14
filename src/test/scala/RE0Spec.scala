package fr.splayce.rel

import org.specs2.mutable._


class RE0Spec extends Specification {

  "RE Escape" should {

    "Escape regex-significant characters" in {
      esc(RE.escapeChars).toString must_==
        (RE.escapeChars map { c => "\\" + c } mkString)
    }

    "Not escape regex-insignificant" in {
      esc("abc")    .toString must_== "abc"
      esc(":=!<>-,").toString must_== ":=!<>-,"
      // by themselves, those do not need escaping
    }

    "Match escaped string literally" in {
      val str = "ab\\c+++.***[]$^d(o{k})<?>:=!-,"
      str must be matching (esc(str).toString)
    }

    "Match escaped regex literally" in {
      val r = "a?".r
      r.toString must be matching (esc(r.toString).toString)
    }

    "Be creatable from symbols" in {
      "abc" must be matching (RE('abc).toString)
    }
  }

}