package fr.splayce.rel.matchers

import org.specs2.mutable._
import _root_.fr.splayce.rel
import rel.Implicits.{RE2Regex, string2RE}


class MatcherSpec extends Specification {

  import _root_.fr.splayce.rel.esc

  "escaped() matchers utility" should {
    "protect the escape prefix" in {
      escaped("w").toString       must_== """(?:(?:\\{2})*\\)(?:w)""" // escape regex-significant
      escaped("w", "$").toString  must_== """(?:(?:\${2})*\$)(?:w)""" // escape regex-significant
      escaped("w", "__").toString must_== """(?:(?:(?:__){2})*(?:__))(?:w)""" // NC group (length > 1)
    }
    "match escaped expression" in {
      val we = escaped("w")

      """w"""     must not be matching(we)
      """\w"""    must     be matching(we)
      """\\w"""   must not be matching(we)
      """\\\w"""  must     be matching(we)
      """\\\\w""" must not be matching(we)

      """\w\w"""  must     be matching(escaped(esc("""w\w""")))
    }
    "respect escape expression" in {
      """\w$w"""   must be matching(escaped(esc("w$w")))
      """$w"""     must be matching(escaped(esc("w"), "$"))
      """$w$w"""   must be matching(escaped(esc("w$w"), "$"))

      """\w$$w"""  must be matching(escaped(esc("w$$w")))
      """$$w"""    must be matching(escaped(esc("w"), "$$"))
      """$$w$$w""" must be matching(escaped(esc("w$$w"), "$$"))
    }
    "use LookBehind when required (not supported in Java)" in {
      escaped("w", lb = true).toString must_== """(?<=(?:\\{2})*\\)(?:w)"""
      escaped("w", lb = true).r should throwA[java.util.regex.PatternSyntaxException]
    }
  }

  "unescaped() matchers utility" should {
    "protect the escape prefix" in {
      unescaped("w").toString       must_== """(?:(?:\\{2})*)(?:w)""" // escape regex-significant
      unescaped("w", "$").toString  must_== """(?:(?:\${2})*)(?:w)""" // escape regex-significant
      unescaped("w", "__").toString must_== """(?:(?:(?:__){2})*)(?:w)""" // NC group (length > 1)
    }
    "match escaped expression" in {
      val wu = unescaped("w")

      """w"""     must     be matching(wu)
      """\w"""    must not be matching(wu)
      """\\w"""   must     be matching(wu)
      """\\\w"""  must not be matching(wu)
      """\\\\w""" must     be matching(wu)

      """w\w"""   must     be matching(unescaped(esc("""w\w""")))
    }
    "respect escape expression" in {
      """w$w"""   must be matching(unescaped(esc("w$w")))
      """w"""     must be matching(unescaped("w", "$"))
      """w$w"""   must be matching(unescaped(esc("w$w"), "$"))

      """w$$w"""  must be matching(unescaped(esc("w$$w")))
      """w"""     must be matching(unescaped("w", "$$"))
      """w$$w"""  must be matching(unescaped(esc("w$$w"), "$$"))
    }
    "use LookBehind when required (not supported in Java)" in {
      unescaped("w", lb = true).toString must_== """(?<=(?:\\{2})*)(?:w)"""
      unescaped("w", lb = true).r should throwA[java.util.regex.PatternSyntaxException]
    }
  }

}