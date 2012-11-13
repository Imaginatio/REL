package fr.splayce.REL

import org.specs2.mutable._


class RE0Spec extends Specification {

  "RE Literal" should {

    "Escape regex-significant characters" in {
      RE.literal(RE.escapeChars).toString must_==
        (RE.escapeChars map { c => "\\" + c } mkString)
    }
    
    "Not escape regex-insignificant" in {
      RE.literal("abc")    .toString must_== "abc"
      RE.literal(":=!<>-,").toString must_== ":=!<>-,"
      // by themselves, ^ those do not need escaping
    }

    "Match literally" in {
      val str = "ab\\c+++.***[]$^d(o{k})<?>:=!-,"
      str must be matching (RE.literal(str).toString)
    }

    "Be creatable from symbols" in {
      "abc" must be matching (RE('abc).toString)
    }
  }

}