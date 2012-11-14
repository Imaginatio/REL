package fr.splayce.rel

import util.Flavor

package object flavors {

  val DotNETFlavor     = Flavor(DotNETTranslator    .translate)
  val JavaScriptFlavor = Flavor(JavaScriptTranslator.translate)

}
