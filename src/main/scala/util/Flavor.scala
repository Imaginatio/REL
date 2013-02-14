package fr.splayce.rel.util

import scala.util.matching.Regex
import Regex.Match

import fr.splayce.rel._


trait FlavorLike {

  def translator: Rewriter

  protected def tr = translator

  def translate(re: RE): RE =
    re map tr
}


abstract class Flavor(val name: String) extends Function1[RE, RE] with FlavorLike {

  def translator: Rewriter

  override lazy val tr = translator

  def apply(re: RE) = translate(re)

  def express(re: RE): (String, List[String]) =
    translate(re).lin


  def compose(that: Flavor): Flavor = {
    val trThis = this.translate(_)
    val trThat = that.translate(_)
    Flavor.from(trThis compose trThat)
  }
  def andThen(that: Flavor): Flavor = that compose this

  protected def notSupported(feature: String, plural: Boolean = false) =
    throw new IllegalArgumentException((feature
        :: (if (plural) "are" else "is")
        :: "not supported in" :: name :: Nil) mkString " ")

}

object Flavor {

  def apply(trans: Rewriter) = new Flavor("<anonymous>") {
    override val translator: Rewriter = trans
  }

  def from(trans: RE => RE) = new Flavor("<anonymous>") {
    override val translator: Rewriter = { case s => trans(s) }
  }

  def from(f: FlavorLike) = apply(f.translator)

}