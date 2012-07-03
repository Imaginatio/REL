package fr.splayce.REL

import scala.util.matching.Regex
import Regex.Match
import org.specs2.matcher.{Matcher, BeTypedEqualTo => Be, BeMatchingRegex, MustMatchers}
import MustMatchers.Descriptible


package object matchers {

  protected[matchers] def haveGroup(groupName: String, value: String = null): Matcher[Option[Match]] = {
    val cond = if (value == null) new Be[Option[String]](None).not
      else new Be[Option[String]](Some(value))
    cond ^^ { (ma: Option[Match]) => ma.flatMap(util.Extractor.get(_, groupName)) }
  }

  protected[matchers] def allBeMatching(re: Regex): Matcher[Seq[String]] =
    new BeMatchingRegex(re).forall

  protected[matchers] def haveSingleDate(res: String*): Matcher[Iterator[List[DateExtractor.Result]]] = {
    (
      (new Be[Int](1) ^^ { (t: List[_]) => new Descriptible(t.size) aka "the number of matched dates" }) and
      (new Be[Seq[String]](res) ^^ { (rs: List[List[DateExtractor.Result]]) =>
        rs.foldLeft(List[String]()) { (acc: List[String], l: List[DateExtractor.Result]) =>
          acc ::: (l.map(_.toString)) } toSeq })
    ) ^^ { (matches: Iterator[List[DateExtractor.Result]]) => matches.toList }
  }
}