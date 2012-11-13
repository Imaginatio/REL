package fr.splayce.REL.util

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.REL._


case class Flavor(translator: Rewriter) {

  def express(re: RE): (String, List[String]) =
    translate(re).lin

  def translate(re: RE): RE =
    re map translator

}
