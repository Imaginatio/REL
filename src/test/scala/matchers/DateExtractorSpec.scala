package fr.splayce.REL.matchers

import org.specs2.mutable._
import _root_.fr.splayce.REL.test._


class DateExtractorsSpec extends Specification {

  "Numeric Date Extractor" should {
    val extractAll = DateExtractor.NUMERIC

    "extract and normalize numeric date" in {
      extractAll("21/10/2000") must haveSingleDate("Y2000 Y00 M10 D21")
      extractAll("00-10-1")    must haveSingleDate("Y00 M10 D01")
      extractAll("10 2000")    must haveSingleDate("Y2000 Y00 M10")
      extractAll("2000 11")    must haveSingleDate("Y2000 Y00 M11")
      extractAll("1999")       must haveSingleDate("Y1999 Y99")
      extractAll("abc")        must be empty
    }

    "not extract impossible days (leap years not accounted)" in {
      extractAll("32-01-2000") must haveSingleDate("Y2000 Y00 M01")
      extractAll("30-02-2000") must haveSingleDate("Y2000 Y00 M02")
      extractAll("32-03-2000") must haveSingleDate("Y2000 Y00 M03")
      extractAll("31-04-2000") must haveSingleDate("Y2000 Y00 M04")
      extractAll("32-05-2000") must haveSingleDate("Y2000 Y00 M05")
      extractAll("31-06-2000") must haveSingleDate("Y2000 Y00 M06")
      extractAll("32-07-2000") must haveSingleDate("Y2000 Y00 M07")
      extractAll("32-08-2000") must haveSingleDate("Y2000 Y00 M08")
      extractAll("31-09-2000") must haveSingleDate("Y2000 Y00 M09")
      extractAll("32-10-2000") must haveSingleDate("Y2000 Y00 M10")
      extractAll("31-11-2000") must haveSingleDate("Y2000 Y00 M11")
      extractAll("32-12-2000") must haveSingleDate("Y2000 Y00 M12")

      extractAll("01-13-2000") must haveSingleDate("Y2000 Y00")

      // 2012 is a leap year, 2011 isn't but it is not taken into account
      extractAll("29-02-2012") must haveSingleDate("Y2012 Y12 M02 D29")
      extractAll("29-02-2011") must haveSingleDate("Y2011 Y11 M02 D29")
    }

    "preserve YY-M-D / D-M-YY ambiguity when 10 < YY <= 31 and D >= 10 and D != Y" in {
      extractAll("00-11-10") must haveSingleDate("Y00 M11 D10") // 00 is Y
      extractAll("01-11-10") must haveSingleDate("Y01 M11 D10") // 01 is Y (YY-M[M]-DD not allowed, must have YYYY)
      extractAll("10-11-10") must haveSingleDate("Y10 M11 D10") // D == Y
      extractAll("10-11-12") must haveSingleDate("Y10 M11 D12", "Y12 M11 D10")
      extractAll("10-2-12")  must haveSingleDate("Y10 M02 D12", "Y12 M02 D10")
      extractAll("10-2-30")  must haveSingleDate("Y30 M02 D10") // feb => max 29 days
      extractAll("31-11-10") must haveSingleDate("Y31 M11 D10") // nov => 30 days
    }

    "not be subject to DD-MM-YYYY / MM-DD-YYYY ambiguity" in {
      extractAll("01-02-2000") must haveSingleDate("Y2000 Y00 M02 D01")
    }
  }

  "Numeric-US Date Extractor" should {
    val extractAll = new DateExtractor(Date.NUMERIC_US.r)

    "extract and normalize numeric date" in {
      extractAll("21/10/2000") must haveSingleDate("Y2000 Y00 M10 D21")
      extractAll("00-10-1")    must haveSingleDate("Y00 M10 D01")
      extractAll("10 2000")    must haveSingleDate("Y2000 Y00 M10")
      extractAll("2000 11")    must haveSingleDate("Y2000 Y00 M11")
      extractAll("1999")       must haveSingleDate("Y1999 Y99")
    }

    "preserve YY-M-D / D-M-YY ambiguity when 10 < YY <= 31 and D >= 10 and D != Y" in {
      extractAll("00-11-10") must haveSingleDate("Y00 M11 D10") // 00 is Y
      extractAll("01-11-10") must haveSingleDate("Y01 M11 D10") // 01 is Y (YY-M[M]-DD not allowed, must have YYYY)
      extractAll("20-11-20") must haveSingleDate("Y20 M11 D20") // D == Y
      extractAll("20-11-12") must haveSingleDate("Y20 M11 D12", "Y12 M11 D20")
      extractAll("20-2-12")  must haveSingleDate("Y20 M02 D12", "Y12 M02 D20")
      extractAll("20-2-30")  must haveSingleDate("Y30 M02 D20") // feb => max 29 days
      extractAll("31-11-10") must haveSingleDate("Y31 M11 D10") // nov => 30 days
    }

    "preserve M-D-YY / D-M-YY ambiguity when D <= 12 and D != M" in {
      extractAll("1-1-42") must haveSingleDate("Y42 M01 D01") // D == Y
      extractAll("13-2-42") must haveSingleDate("Y42 M02 D13") // 13 is DD
      extractAll("2-13-42") must haveSingleDate("Y42 M02 D13") // 13 is DD
      extractAll("1-2-42") must haveSingleDate("Y42 M01 D02", "Y42 M02 D01")
    }

    "preserve multiple M-D-YY / YY-M-D / D-M-YY ambiguity" in {
      extractAll("11-11-11") must haveSingleDate("Y11 M11 D11") // D == M == Y
      extractAll("9-8-09")   must haveSingleDate("Y09 M09 D08", "Y09 M08 D09") // 09 == YY
      extractAll("10-11-12") must haveSingleDate("Y12 M10 D11", "Y10 M11 D12", "Y12 M11 D10")
    }

    "preserve MM-DD-YYYY / DD-MM-YYYY ambiguity when DD <= 12 and DD != MM" in {
      extractAll("13-02-2000") must haveSingleDate("Y2000 Y00 M02 D13") // 13 is DD
      extractAll("02-13-2000") must haveSingleDate("Y2000 Y00 M02 D13") // 13 is DD
      extractAll("02-02-2000") must haveSingleDate("Y2000 Y00 M02 D02") // DD == MM
      extractAll("01-02-2000") must haveSingleDate("Y2000 Y00 M01 D02", "Y2000 Y00 M02 D01")
    }
  }

  "French date extractor" should {
    import fr.DateExtractor.{apply => extractAll}

    "still extract and normalize numeric date" in {
      extractAll("01/10/2000") must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("00-10-1")    must haveSingleDate("Y00 M10 D01")
      extractAll("10 2000")    must haveSingleDate("Y2000 Y00 M10")
      extractAll("2000 11")    must haveSingleDate("Y2000 Y00 M11")
      extractAll("1999")       must haveSingleDate("Y1999 Y99")
    }
    "extract and normalize alpha date" in {
      extractAll("1 octobre 2000")   must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("1er octobre 2000") must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("01oct2000")        must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("1 oct. 00")        must haveSingleDate("Y00 M10 D01")
      extractAll("novembre 2000")    must haveSingleDate("Y2000 Y00 M11")
      extractAll("nov2000")          must haveSingleDate("Y2000 Y00 M11")
      extractAll("nov. 2000")        must haveSingleDate("Y2000 Y00 M11")
      extractAll("novembre")         must haveSingleDate("M11")
      extractAll("nov")              must haveSingleDate("M11")
      extractAll("nov.")             must haveSingleDate("M11")
    }
  }

  "English date extractor" should {
    import en.DateExtractor.{apply => extractAll}

    "still extract and normalize numeric (US) date" in {
      extractAll("10/21/2000") must haveSingleDate("Y2000 Y00 M10 D21")
      extractAll("00-10-1")    must haveSingleDate("Y00 M10 D01")
      extractAll("10 2000")    must haveSingleDate("Y2000 Y00 M10")
      extractAll("2000 11")    must haveSingleDate("Y2000 Y00 M11")
      extractAll("1999")       must haveSingleDate("Y1999 Y99")
    }
    "extract and normalize alpha date" in {
      //extractAll("1 october 2000")    must haveSingleDate("Y2000 Y00 M10 D01")
      //extractAll("1st october, 2000") must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("october 1 2000")    must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("october 1st, 2000") must haveSingleDate("Y2000 Y00 M10 D01")
      //extractAll("01oct2000")         must haveSingleDate("Y2000 Y00 M10 D01")
      extractAll("oct. 1, 00")        must haveSingleDate("Y00 M10 D01")
      extractAll("november 2000")     must haveSingleDate("Y2000 Y00 M11")
      extractAll("nov2000")           must haveSingleDate("Y2000 Y00 M11")
      extractAll("nov. 2000")         must haveSingleDate("Y2000 Y00 M11")
      extractAll("november")          must haveSingleDate("M11")
      extractAll("nov")               must haveSingleDate("M11")
      extractAll("nov.")              must haveSingleDate("M11")
    }
  }
}