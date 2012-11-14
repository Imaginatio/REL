package fr.splayce.rel.util

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.rel._


case class Flavor(translator: Rewriter) extends Function1[RE, RE] {
  def this(translate: RE => RE) =
    this(translator = { case s => translate(s) })

  def express(re: RE): (String, List[String]) =
    translate(re).lin

  def translate(re: RE): RE =
    re map translator

  def apply(re: RE) = translate(re)

  def compose(that: Flavor): Flavor = {
    val trThis = this.translate(_)
    val trThat = that.translate(_)
    new Flavor(trThis compose trThat)
  }
  def andThen(that: Flavor): Flavor = that compose this

}
