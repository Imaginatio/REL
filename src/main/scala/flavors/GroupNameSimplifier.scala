package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.{Flavor, Rewriter}

import scala.util.matching.Regex


/** Group name simplifier for Flavors that support limited group names.
 *  @see [[fr.splayce.rel.flavors.EmbedGroupNames]]
 */
class GroupNameSimplifier(val strip: Regex, val replacement: String = "") extends Flavor("GroupNameSimplifier") {

  override val translator: Rewriter = {
    case Group(name, re, es) =>
      val alt = strip.replaceAllIn(name, replacement)
      if (alt.isEmpty)
        Group(name, re, None)
      else
        Group(alt, re, es)
    case r @ GroupRef(name, _) =>
      val alt = strip.replaceAllIn(name, replacement)
      if (alt == name)
        r
      else if (alt.isEmpty)
        r.copy(embedStyle = None)
      else
        r.copy(name = alt)
  }

}

object GroupNameSimplifier {

  lazy val strict  = new GroupNameSimplifier("""^[^a-zA-Z]+|[^a-zA-Z0-9]+""".r)
  lazy val snake   = new GroupNameSimplifier("""^[^a-zA-Z]+|\W+""".r)
  lazy val lenient = new GroupNameSimplifier("""^[^a-zA-Z_]+|[^ -=?-~]+""".r)

}