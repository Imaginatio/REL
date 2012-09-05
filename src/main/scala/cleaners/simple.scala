package fr.splayce.REL.cleaners


trait Cleaner {
  def clean(in: String): String
  def apply(in: String) = clean(in)

  def apply(cleaner: Cleaner) =
    this match {
      case ChainedCleaner(cleaners) => ChainedCleaner(cleaner :: cleaners)
      case _ => ChainedCleaner(this :: cleaner :: Nil)
    }
    
}


case class ChainedCleaner(cleaners: List[Cleaner]) extends Cleaner {
  require(cleaners.size > 0)

  override def clean(in: String) = applyCleaners(cleaners, in)
  
  def applyCleaners(cleaners: List[Cleaner], in: String): String =
    cleaners match {
      case Nil => in
      case List(cleaner) => cleaner(in)
      case cleaner :: tail => cleaner(applyCleaners(tail, in))
    }
}

  
object IdentityCleaner extends Cleaner {
  override def clean(in: String) = in
}

object WhiteSpaceCleaner extends Cleaner {
  val WhiteSpaces = """\s+""".r
  override def clean(in: String) =
    WhiteSpaces.pattern.matcher(in).replaceAll(" ")
}

object CamelCaseSplitter extends Cleaner {
  val CamelCase = """(\p{Ll})(?=\p{Lu}\p{Ll})""".r
  override def clean(in: String) =
    CamelCase.pattern.matcher(in).replaceAll("$1 ")
}

object LowerCaseFilter extends Cleaner {
  override def clean(in: String) = in.toLowerCase
}
