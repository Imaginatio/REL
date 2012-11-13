package fr.splayce.REL

import util.Flavor

package object flavors {

  val DotNETFlavor     = Flavor(DotNETTranslator    .translate)
  val JavaScriptFlavor = Flavor(JavaScriptTranslator.translate)

}
