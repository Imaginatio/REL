package fr.splayce.REL.cleaners

import fr.splayce.REL.util.Cleaner

 
object IdentityCleaner extends Cleaner {
  override def clean(in: String) = in
}


object WhiteSpaceCleaner extends Cleaner {
  val WhiteSpaces = """\s+""".r
  
  override def clean(in: String) = WhiteSpaces.replaceAllIn(in, " ")
}


object CamelCaseSplitter extends Cleaner {
  val CamelCase = """(\p{Ll})(?=\p{Lu}\p{Ll})""".r

  override def clean(in: String) = CamelCase.replaceAllIn(in, "$1 ")
}


object LowerCaseFilter extends Cleaner {
  override def clean(in: String) = in.toLowerCase
}
