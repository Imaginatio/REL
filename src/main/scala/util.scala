package fr.splayce.REL.util

import scala.util.matching.Regex
import Regex.Match


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

