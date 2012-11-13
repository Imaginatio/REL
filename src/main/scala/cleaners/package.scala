package fr.splayce.REL

import fr.splayce.REL.util.Cleaner


package object cleaners {

  val IdentityCleaner = Cleaner { in => in }

  val LowerCaseFilter = Cleaner { _ toLowerCase }

  val WhiteSpaceCleaner = Cleaner {
    val WhiteSpaces = """\s+""".r
    (in: String) => WhiteSpaces.replaceAllIn(in, " ")
  }

  val CamelCaseSplitter = Cleaner {
    val CamelCase = """(\p{Ll})(?=\p{Lu}\p{Ll})""".r
    (in: String) => CamelCase.replaceAllIn(in, "$1 ")
  }

  val DiacriticCleaner = Cleaner(DiacriticFolder.clean _)
  
}
