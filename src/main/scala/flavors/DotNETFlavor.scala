package fr.splayce.rel.flavors

import fr.splayce.rel._
import util.{Flavor, Rewriter}


/** ․NET flavor
 *
 *  This flavor:
 *  - embedds group names in regex (`(?<name>expr)` / `\k<name>` syntax)
 *  - convert possessive quantifiers to greedy in atomic groups
 *  - translate `\w` (when referenced by `μ` / `Word`) into `[a-zA-Z0-9_]`
 *    because .NET's `\w` would also matches letters with diacritics
 *    while Java's `\w` only matches ASCII letters
 *    (use `\p{L}` insead with `λ` / `Letter` for all Unicode letters)
 *
 *  @see [[fr.splayce.rel.flavors.PossessiveToAtomic]]
 *  @see [[http://www.regular-expressions.info/dotnet.html .NET regex flavor]],
 */
object DotNETFlavor extends Flavor(".NET") with EmbedGroupNames with PossessiveToAtomic {

  private val ASCIIWord    = new TranslatedRECst("[a-zA-Z0-9_]")
  private val NotASCIIWord = new TranslatedRECst("[^a-zA-Z0-9_]")

  override val translator: Rewriter = {

    // .NET's \w would also match letters with diacritics
    case Word               => DotNETFlavor.ASCIIWord
    case NotWord            => DotNETFlavor.NotASCIIWord

  }

}