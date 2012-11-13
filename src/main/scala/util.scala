package fr.splayce.REL.util

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.REL._


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


trait Extractor[T] {
  val regex: Regex

  def extractMatch(m: Match): Option[T]

  def extractAll(in: String): Iterator[T] =
    regex.findAllIn(in).matchData.flatMap(extractMatch(_))
}

object Extractor {

  def get(ma: Regex.Match, groupName: String): Option[String] =
    Option(ma.group(groupName)).filterNot(_.isEmpty)

  def has(ma: Regex.Match, groupName: String): Boolean =
    get(ma, groupName).isDefined
}


trait GroupExtractor[T] extends Extractor[T] {

  def convertMatch(m: String): Option[T]

  def extractMatch(m: Match)(implicit convert: String => Option[T]): Option[T] =
    if (m.groupCount > 0) Some(m.group(1)).filterNot(_.isEmpty).flatMap(convertMatch(_)) else None
}



abstract class Flavor {

  def express(re: RE): (String, List[String]) =
    translate(re).linear()

  /** Returns a recursively translation of the given RE (sub)tree.
    *
    * This is the method to override when implementing a Flavor;
    * the default case being typically:
    * {{{
    * case _ => super.translate(re)
    * }}}
    */
  def translate(re: RE): RE = re match {

    // repeaters
    case         Opt(re,       mode) =>         Opt(translate(re),       mode)
    case       KStar(re,       mode) =>       KStar(translate(re),       mode)
    case      KCross(re,       mode) =>      KCross(translate(re),       mode)
    case     RepNToM(re, n, m, mode) =>     RepNToM(translate(re), n, m, mode)
    case RepAtLeastN(re, n,    mode) => RepAtLeastN(translate(re), n,    mode)
    case  RepAtMostN(re, n,    mode) =>  RepAtMostN(translate(re), n,    mode)
    case RepExactlyN(re, n         ) => RepExactlyN(translate(re), n         )

    // other RE1 (grouping)
    case Group(name, re)       => Group(name, translate(re))
    case      AGroup(re)       =>      AGroup(translate(re))
    case     NCGroup(re)       =>     NCGroup(translate(re))
    case  LookAround(re, d, p) =>  LookAround(translate(re), d, p)

    // RE2
    case Conc(re1, re2) => Conc(translate(re1), translate(re2))
    case  Alt(re1, re2) =>  Alt(translate(re1), translate(re2))

    case _ => re
  }
}
