package fr.splayce.REL

object TestRun {

  def tests = {

    import scala.util.matching.Regex
    import scala.util.matching.Regex.Match
    import scala.util.matching.Regex.MatchIterator

    import Implicits._
    import Symbols._
    
    val DATE_SEP = "[ /._-]"
    val s  = "sep"
    val S  = DATE_SEP \ s
    
    val YYYY = ("19" | "20") ~ δ ~ δ
    val MM   = "0[1-9]" | "1[012]"
    val DD   = "0[1-9]" | ("[12][0-9]" | "3[01]")
    
    val YY = δ ~ δ
    val M  = "[1-9]" | "1[012]"
    val D  = "[1-9]" | ("[12][0-9]" | "3[01]")
    
    val DATE_YMD_L = YYYY ~ S ~ MM ~ !S ~ DD
    val DATE_DMY_L = DD   ~ S ~ MM ~ !S ~ YYYY
    
    val DATE_YMD_S = YY ~ S ~ M ~ !S ~ D
    val DATE_DMY_S = D  ~ S ~ M ~ !S ~ YY
    
    val DATE_YMD = DATE_YMD_L \ "ymd_long" | DATE_YMD_S \ "ymd_short"
    val DATE_DMY = DATE_DMY_L \ "dmy_long" | DATE_DMY_S \ "dmy_short"
    
    val DATE = (DATE_YMD \ "ymd" | DATE_DMY \ "dmy") \ "d"
    
    println( "----" )
    
    val text = ("BLAH 2012-12-03 " +
      "bla té 08-05-2012 " +
      "ou € alors 10/05/2012-11/05/2012" +
      "mais aussi 5.5.12.\n" +
      "Et Enfin 1 1 11.")

    val ms = DATE findAllIn text

    val matchFormat: (Match, MatchIterator) => String = {
      (m, mi) => mi.groupNames.foldLeft("") (
    (acc, n) => (n, m.group(n)) match {
      case (_, null) => acc
      case (n, g)    => acc + "\n" + n + "\t" + g
    }
      )
    }

    val matchPrint: (Match, MatchIterator, (Match, MatchIterator) => String) => Unit = {
      (m, ms, f) => println(f(m, ms))
    }

    println (DATE)
    println ("GCount : " + ms.groupCount)
    println ("GNames : " + ms.groupNames)
    
    ms.matchData foreach (matchPrint(_, ms, matchFormat))
    
    println("----")

    // val r = """(a)b\1|(a)\2""".r
    // val t = "aa aba ba"
    // val m = r findAllIn t
    // m.matchData foreach (m => println(m.group(0)) )

    // val R = "a" ~ S ~ "b" ~ SBR ~ "c"
    // val txt = "a-b.c"
    // val ms = R findAllIn txt
    // println( R )
    // println( "GCount : " + ms.groupCount )
    // println( "GNames : " + ms.groupNames )
    // ms.matchData.foreach( m => println("Match : \"" + m + "\"") )
    // println("----")
    
    // val Q: RE = "'"
    // val A: RE = "."
    // val BLA = Q ~ ((A.++ ~ "'") \ "ga")
    // val text = "'blaa'"
    // val ms = BLA findAllIn text
    // println( BLA )
    // println( "GCount : " + ms.groupCount )
    // println( "GNames : " + ms.groupNames )
    // ms.matchData.foreach( m => println("Match : \"" + m.group("ga") + "\"") )
    // println("----")

  }


  // val A = """[aA]""".r
  // val B = """[bB]""".r
  // val C = """[cC]""".r
  // val Sep = "[ |/-]".r
  // val g1 = "g1"
  // val g2 = "g2"
  // val X = (B\g1 +) ~ Sep ~ (C\g2 +) // | C ~ A.>! | B | A
  // println("----")
  // println ( "X : " + X.asInstanceOf[AnyRef].getClass.getSimpleName  )
  // println ( "X = \"" + X + "\"")
  // println("----")
  //val text = "b-c B|C"
  // println( "GCount : " + ms.groupCount )
  // println( "GNames : " + ms.groupNames )
  // ms.matchData.foreach( m => println("Match : \"" +
  //                m.group(g1) +
  //                ", " +
  //                m.group(g2) + "\"") )
  // val opt = for (m <- X findFirstMatchIn text) yield m group "g1"
  // println( opt )
  // val date = new Regex("""(\d\d\d\d)-(\d\d)-(\d\d)""", "year", "month", "day")
  // val dtext = "2012-05-10"
  // val dms = date findAllIn dtext
  // println( "GCount : " + dms.groupCount )
  // println( "GNames : " + dms.groupNames )
  // val dopt = for (m <- date findFirstMatchIn dtext) yield m group "year"
  // println( dopt )
  // val Y : Regex = new Regex("""(\w)""", "g")
  // println ( "Y = \"" + Y +"\"")
  // val m = Y.findFirstMatchIn("caab").get
  // println( m.group("g") )
  // println("----")

  // import java.util.regex._
  // val t = "aa"
  // val p: Pattern = Pattern.compile("""(?<TEST>a*)""")
  // val m: Matcher = p.matcher(t)
  // if(m.matches()) {
  //   println( m.group("TEST") )
  // } else {
  //   println( "Does not match" )
  // }
  
  //"ɸβfvθðszʃʒʂʐçʝxɣχʁħʕʜ".map(c => "%s\t\\u%04X".format(c, c.toInt)) foreach println


}