package fr.splayce.REL.util

import scala.util.matching.Regex


case class Cleaner(val clean: String => String) extends Function1[String, String] {

  def apply(in: String) = clean(in)

  // function-like syntax: chain = Third(Second(First))
  def apply(previous: Cleaner) = Cleaner(clean compose previous.clean)
  def compose(previous: Cleaner) = this(previous)

  // Unix/pipe-like syntax: chain = First | Second | Third
  def |(then: Cleaner) = Cleaner(clean andThen then.clean)
  def andThen(then: Cleaner) = this | then

}


object Cleaner {

  def regexReplaceAll(re: Regex, replacement: String) =
    Cleaner { in => re.replaceAllIn(in, replacement) }

}