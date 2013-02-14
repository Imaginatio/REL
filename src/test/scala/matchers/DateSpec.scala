package fr.splayce.rel.matchers

import org.specs2.mutable._

import _root_.fr.splayce.rel
import rel.Implicits.RE2Regex
import rel.test._


class DateSpec extends Specification {

  "Numeric date regex" should {
    import Date.NUMERIC

    "not match empty string" in {
      "" must not be matching(NUMERIC)
    }

    "match lonely 4-digits year of 19th to 21st centuries" in {
      "1699" must not be matching(NUMERIC)
      "1700" must     be matching(NUMERIC)
      "1799" must     be matching(NUMERIC)
      "1800" must     be matching(NUMERIC)
      "1899" must     be matching(NUMERIC)
      "1900" must     be matching(NUMERIC)
      "1999" must     be matching(NUMERIC)
      "2000" must     be matching(NUMERIC)
      "2099" must     be matching(NUMERIC)
      "2100" must not be matching(NUMERIC)
      NUMERIC.findAllIn("test 1996 98 2004 12 ok 2012") must have size(3)
    }

        "match D-M-YY"     in { "1-1-00"     must     be matching(NUMERIC) }
        "match D-M-YYYY"   in { "1-1-2000"   must     be matching(NUMERIC) }
        "match DD-MM-YYYY" in { "01-10-2000" must     be matching(NUMERIC) }
    "not match DD-MM-YY"   in { "01-10-00"   must not be matching(NUMERIC) }
    "not match XX-MM-YY where XX can't be a day" in {
      "00-10-00" must not be matching(NUMERIC)
      "32-10-00" must not be matching(NUMERIC)
      "32-10-00" must not be matching(NUMERIC)
      "41-10-00" must not be matching(NUMERIC)
      "99-10-00" must not be matching(NUMERIC)
    }

        "match YY-M-D"     in { "00-1-1"     must     be matching(NUMERIC) }
        "match YYYY-M-D"   in { "2000-1-1"   must     be matching(NUMERIC) }
        "match YYYY-MM-DD" in { "2000-10-01" must     be matching(NUMERIC) }
    "not match YY-MM-DD"   in { "00-10-01"   must not be matching(NUMERIC) }

    "not match M-DD-YY"    in { "1-21-00"    must not be matching(NUMERIC) }
    "not match MM-DD-YY"   in { "01-21-00"   must not be matching(NUMERIC) }
        "match M-DD-YYYY"  in { "1-21-2000"  must not be matching(NUMERIC) }
    "not match MM-DD-YYYY" in { "01-21-2000" must not be matching(NUMERIC) }

        "match MM-YYYY" in { "01-2000" must     be matching(NUMERIC) }
        "match YYYY-MM" in { "2000-01" must     be matching(NUMERIC) }
    "not match M-YY"    in { "1-00"    must not be matching(NUMERIC) }
    "not match YY-M"    in { "00-1"    must not be matching(NUMERIC) }

    "match with separators [ -/._]" in {
      "01 10 2000" must be matching(NUMERIC)
      "01-10-2000" must be matching(NUMERIC)
      "01/10/2000" must be matching(NUMERIC)
      "01.10.2000" must be matching(NUMERIC)
      "01_10_2000" must be matching(NUMERIC)
    }
    "not match with unmatched separators [ -/._]" in {
      "01 10/2000" must not be matching(NUMERIC)
      "01-10.2000" must not be matching(NUMERIC)
      "01/10_2000" must not be matching(NUMERIC)
      "01.10 2000" must not be matching(NUMERIC)
      "01_10-2000" must not be matching(NUMERIC)
    }

    "match YMD > DMY when ambiguous" in {
      (NUMERIC findFirstMatchIn "22-11-2011") must haveGroup("n_dmy", "22-11-2011")
      (NUMERIC findFirstMatchIn "22-1-11")    must haveGroup("n_ymd", "22-1-11")
      (NUMERIC findFirstMatchIn "22-1-41")    must haveGroup("n_dmy", "22-1-41")
    }

    "match date or year inside a word" in {
      (NUMERIC findAllIn        "test2012ok")       must have size(1)
      (NUMERIC findAllIn        "test01-03-2012ok") must have size(1)
      (NUMERIC findAllIn        "test03-2012ok")    must have size(1)
      (NUMERIC findAllIn        "test1-3-12ok")     must have size(1)
      (NUMERIC findAllIn        "test1-3-2012ok")   must have size(1)
      (NUMERIC findFirstMatchIn "test2012ok")       must haveGroup("n_y", "2012")
      (NUMERIC findFirstMatchIn "test01-03-2012ok") must haveGroup("n_dmy", "01-03-2012")
      (NUMERIC findFirstMatchIn "test03-2012ok")    must haveGroup("n_my", "03-2012")
      (NUMERIC findFirstMatchIn "test1-3-12ok")     must haveGroup("n_dmy", "1-3-12")
      (NUMERIC findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_dmy", "1-3-2012")
    }
    "not match inside a number" in {
      (NUMERIC findAllIn   "test02012ok")            must be empty;
      (NUMERIC findAllIn   "test20120ok")            must be empty;
      (NUMERIC findAllIn   "test01-03-20120ok")      must be empty;
      (NUMERIC findAllIn   "test03-20120ok")         must be empty;
      (NUMERIC findAllIn   "test1-3-120ok")          must be empty;
      (NUMERIC findAllIn   "test03-12ok")            must be empty;
      (NUMERIC findFirstIn "test01-3-42ok")          must be empty;
      // 01-3-12 is possible, interpreted as short form of 2001-03-12
      (NUMERIC findFirstMatchIn "test01-3-12ok")     must haveGroup("n_ymd", "01-3-12")
      (NUMERIC findFirstMatchIn "test001-03-2012ok") must haveGroup("n_my", "03-2012")
      (NUMERIC findFirstMatchIn "test003-2012ok")    must haveGroup("n_y",  "2012")
    }
  }

  "Numeric full date regex" should {
    import Date.NUMERIC_FULL

    "still match numeric" in {
      List("01/10/2000", "00-10-1", "01 10 2000") must allBeMatching(NUMERIC_FULL)
      (NUMERIC_FULL findAllIn        "test01-03-2012ok") must have size(1)
      (NUMERIC_FULL findAllIn        "test1-3-12ok")     must have size(1)
      (NUMERIC_FULL findAllIn        "test1-3-2012ok")   must have size(1)
      (NUMERIC_FULL findFirstMatchIn "test01-03-2012ok") must haveGroup("n_dmy", "01-03-2012")
      (NUMERIC_FULL findFirstMatchIn "test1-3-12ok")     must haveGroup("n_dmy", "1-3-12")
      (NUMERIC_FULL findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_dmy", "1-3-2012")
      (NUMERIC_FULL findFirstMatchIn "test21-3-12ok")    must haveGroup("n_ymd", "21-3-12")
      (NUMERIC_FULL findFirstMatchIn "test22-1-41ok")    must haveGroup("n_dmy", "22-1-41")
    }
    "not match partial date" in {
      List("10 2000", "2000 11", "1999") must noneBeMatching(NUMERIC_FULL)
      (NUMERIC_FULL findAllIn        "test2012ok")       must be empty;
      (NUMERIC_FULL findAllIn        "test03-2012ok")    must be empty;
    }
  }


  "Numeric-US Date regex" should {
    import Date.NUMERIC_US

    "still match numeric" in {
      List("01/10/2000", "1/3/2000", "00-10-1", "10 2000", "2000 11", "1999") must allBeMatching(NUMERIC_US)
    }

        "match M-DD-YY"    in { "1-21-00"    must     be matching(NUMERIC_US) }
        "match MM-DD-YYYY" in { "01-21-2000" must     be matching(NUMERIC_US) }
    "not match MM-DD-YY"   in { "01-21-00"   must not be matching(NUMERIC_US) }
        "match M-DD-YYYY"  in { "1-21-2000"  must     be matching(NUMERIC_US) }

    "match MDY > YMD > DMY when ambiguous" in {
      (NUMERIC_US findFirstMatchIn "11-11-2011") must haveGroup("n_mdy", "11-11-2011")
      (NUMERIC_US findFirstMatchIn "22-11-2011") must haveGroup("n_dmy", "22-11-2011")
      (NUMERIC_US findFirstMatchIn "1-1-2011")   must haveGroup("n_mdy", "1-1-2011")
      (NUMERIC_US findFirstMatchIn "1-1-11")     must haveGroup("n_mdy", "1-1-11")
      (NUMERIC_US findFirstMatchIn "22-1-11")    must haveGroup("n_ymd", "22-1-11")
      (NUMERIC_US findFirstMatchIn "22-1-41")    must haveGroup("n_dmy", "22-1-41")
    }
  }

  "Numeric-US full date regex" should {
    import Date.NUMERIC_FULL_US

    "still match numeric (with MDY > YMD > DMY)" in {
      List("01/10/2000", "1/3/2000", "00-10-1", "01 10 2000") must allBeMatching(NUMERIC_FULL_US)
      List("1-21-00", "3-21-2000", "01-21-2000")              must allBeMatching(NUMERIC_FULL_US)
      (NUMERIC_FULL_US findFirstMatchIn "01/10/2000") must haveGroup("n_mdy", "01/10/2000")
      (NUMERIC_FULL_US findFirstMatchIn "3/1/2000")   must haveGroup("n_mdy", "3/1/2000")
      (NUMERIC_FULL_US findFirstMatchIn "11-11-2011") must haveGroup("n_mdy", "11-11-2011")
      (NUMERIC_FULL_US findFirstMatchIn "22-11-2011") must haveGroup("n_dmy", "22-11-2011")
      (NUMERIC_FULL_US findFirstMatchIn "1-1-11")     must haveGroup("n_mdy", "1-1-11")
      (NUMERIC_FULL_US findFirstMatchIn "22-1-11")    must haveGroup("n_ymd", "22-1-11")

      (NUMERIC_FULL_US findAllIn        "test01-03-2012ok") must have size(1)
      (NUMERIC_FULL_US findAllIn        "test1-3-12ok")     must have size(1)
      (NUMERIC_FULL_US findAllIn        "test1-3-2012ok")   must have size(1)
      (NUMERIC_FULL_US findFirstMatchIn "test01-03-2012ok") must haveGroup("n_mdy", "01-03-2012")
      (NUMERIC_FULL_US findFirstMatchIn "test21-03-2012ok") must haveGroup("n_dmy", "21-03-2012")
      (NUMERIC_FULL_US findFirstMatchIn "test1-3-12ok")     must haveGroup("n_mdy", "1-3-12")
      (NUMERIC_FULL_US findFirstMatchIn "test21-3-12ok")    must haveGroup("n_ymd", "21-3-12")
      (NUMERIC_FULL_US findFirstMatchIn "test22-1-41ok")    must haveGroup("n_dmy", "22-1-41")
      (NUMERIC_FULL_US findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_mdy", "1-3-2012")
      (NUMERIC_FULL_US findFirstMatchIn "test21-3-2012ok")  must haveGroup("n_dmy", "21-3-2012")
    }
    "not match partial date" in {
      List("10 2000", "2000 11", "1999") must noneBeMatching(NUMERIC_FULL_US)
      (NUMERIC_FULL_US findAllIn        "test2012ok")       must be empty;
      (NUMERIC_FULL_US findAllIn        "test03-2012ok")    must be empty;
    }
  }


  "French date regex" should {
    import fr.Date.{ALL => FR_ALL, ALPHA => FR_ALPHA}

    "not match empty string" in {
      "" must not be matching(FR_ALL)
      "" must not be matching(FR_ALPHA)
    }

    "still match numeric (with YMD > DMY)" in {
      List("01/10/2000", "00-10-1", "10 2000", "2000 11", "1999") must allBeMatching(FR_ALL)
      (FR_ALL findAllIn        "test2012ok")       must have size(1)
      (FR_ALL findAllIn        "test01-03-2012ok") must have size(1)
      (FR_ALL findAllIn        "test03-2012ok")    must have size(1)
      (FR_ALL findAllIn        "test1-3-12ok")     must have size(1)
      (FR_ALL findAllIn        "test1-3-2012ok")   must have size(1)
      (FR_ALL findFirstMatchIn "test2012ok")       must haveGroup("n_y", "2012")
      (FR_ALL findFirstMatchIn "test01-03-2012ok") must haveGroup("n_dmy", "01-03-2012")
      (FR_ALL findFirstMatchIn "test03-2012ok")    must haveGroup("n_my", "03-2012")
      (FR_ALL findFirstMatchIn "test1-3-12ok")     must haveGroup("n_dmy", "1-3-12")
      (FR_ALL findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_dmy", "1-3-2012")
      (FR_ALL findFirstMatchIn "test21-3-12ok")    must haveGroup("n_ymd", "21-3-12")
      (FR_ALL findFirstMatchIn "test22-1-41ok")    must haveGroup("n_dmy", "22-1-41")
    }

    "match months in short form" in {
      // NB missing 'mars', 'mai', 'juin', 'aout'
      // usually not abbreviated
      List(
        "janv", "fev", "fév", "févr", "fevr", "avr", "juil", "juill",
        "sept", "oct", "nov", "dec", "déc"
      ) must allBeMatching(FR_ALL)
      List(
        "janv.", "fev.", "fév.", "févr.", "fevr.", "avr.", "juil.",  "juill.",
        "sept.", "oct.", "nov.", "dec.", "déc."
      ) must allBeMatching(FR_ALL)
    }
    "match months in full form" in {
      List(
        "janvier", "fevrier", "février", "mars",      "avril",   "mai",      "juin",
        "juillet", "aout",    "août",    "septembre", "octobre", "novembre", "decembre", "décembre"
      ) must allBeMatching(FR_ALL)
    }

    "match unseparated month + YY[YY]" in {
      List(
        "nov2012",      "nov12",      "nov'12",
        "nov.2012",     "nov.12",     "nov.'12",
        "novembre2012", "novembre12", "novembre'12"
      ) must allBeMatching(FR_ALL)
    }
    "match month YY[YY]" in {
      List(
        "nov 2012",      "nov 12",      "nov '12",
        "nov. 2012",     "nov. 12",     "nov. '12",
        "novembre 2012", "novembre 12", "novembre '12"
      ) must allBeMatching(FR_ALL)
    }
    "match 'month YY[YY].' in one match without the final dot" in {
      "nov 2012." must not be matching(FR_ALL)
      (FR_ALL findAllIn   "nov 2012.") must have size(1)
      (FR_ALL findFirstIn "nov 2012.") must_== Some("nov 2012")
    }

    "match unseparated D[D] + month" in {
      List(
         "3nov",  "3nov.",  "3novembre",
        "03nov", "03nov.", "03novembre"
      ) must allBeMatching(FR_ALL)
    }
    "match D[D] month" in {
      List(
         "3 nov",  "3 nov.",  "3 novembre",
        "03 nov", "03 nov.", "03 novembre"
      ) must allBeMatching(FR_ALL)
    }

    "match unseparated D[D] + month + YY[YY]" in {
      List(
         "3nov2012",  "3nov12",  "3nov'12",  "3nov.2012",  "3nov.12",  "3nov.'12",
        "03nov2012", "03nov12", "03nov'12", "03nov.2012", "03nov.12", "03nov.'12",
         "3novembre2012",  "3novembre12",  "3novembre'12",
        "03novembre2012", "03novembre12", "03novembre'12"
      ) must allBeMatching(FR_ALL)
    }
    "match D[D] month YY[YY]" in {
      List(
         "3 nov 2012",  "3 nov 12",  "3 nov '12",  "3 nov. 2012",  "3 nov. 12",  "3 nov. '12",
        "03 nov 2012", "03 nov 12", "03 nov '12", "03 nov. 2012", "03 nov. 12", "03 nov. '12",
         "3 novembre 2012",  "3 novembre 12",  "3 novembre '12",
        "03 novembre 2012", "03 novembre 12", "03 novembre '12"
      ) must allBeMatching(FR_ALL)
    }

    "match 'D[D] month YY[YY].' in one match without the final dot" in {
      "1er nov 2012." must not be matching(FR_ALL)
      (FR_ALL findAllIn   "1er nov 2012.") must have size(1)
      (FR_ALL findFirstIn "1er nov 2012.") must_== Some("1er nov 2012")
      "3 nov 2012."   must not be matching(FR_ALL)
      (FR_ALL findAllIn   "3 nov 2012.")   must have size(1)
      (FR_ALL findFirstIn "3 nov 2012.")   must_== Some("3 nov 2012")
      "03 nov 2012."  must not be matching(FR_ALL)
      (FR_ALL findAllIn   "03 nov 2012.")  must have size(1)
      (FR_ALL findFirstIn "03 nov 2012.")  must_== Some("03 nov 2012")
    }

    "not find month inside a word" in {
      (FR_ALL findAllIn   "")          must have size(0)
      (FR_ALL findAllIn   "innovant")  must have size(0)
      (FR_ALL findAllIn   "novateur")  must have size(0)
      (FR_ALL findAllIn   "renov")     must have size(0)
      (FR_ALL findAllIn   "innov.ant") must have size(0)
      (FR_ALL findAllIn   "renov.")    must have size(0)
      (FR_ALL findFirstIn "renov2012") must_== Some("2012")
    }
    "find full date inside a word" in {
      (FR_ALL findAllIn   "re3nov12v3")    must have size(1)
      (FR_ALL findFirstIn "re3nov12v3")    must_== Some("3nov12")
      (FR_ALL findAllIn   "re3nov2012v3")  must have size(1)
      (FR_ALL findFirstIn "re3nov2012v3")  must_== Some("3nov2012")
      (FR_ALL findAllIn   "re03nov2012v3") must have size(1)
      (FR_ALL findFirstIn "re03nov2012v3") must_== Some("03nov2012")
    }
  }

  "French full date regex" should {
    import fr.Date.{ALL_FULL => FR_FULL}

    "still match numeric" in {
      List("01/10/2000", "00-10-1", "01 10 2000") must allBeMatching(FR_FULL)
      (FR_FULL findAllIn        "test01-03-2012ok") must have size(1)
      (FR_FULL findAllIn        "test1-3-12ok")     must have size(1)
      (FR_FULL findAllIn        "test1-3-2012ok")   must have size(1)
      (FR_FULL findFirstMatchIn "test01-03-2012ok") must haveGroup("n_dmy", "01-03-2012")
      (FR_FULL findFirstMatchIn "test1-3-12ok")     must haveGroup("n_dmy", "1-3-12")
      (FR_FULL findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_dmy", "1-3-2012")
      (FR_FULL findFirstMatchIn "test21-3-12ok")    must haveGroup("n_ymd", "21-3-12")
      (FR_FULL findFirstMatchIn "test22-1-41ok")    must haveGroup("n_dmy", "22-1-41")
    }
    "still not match partial numeric date" in {
      List("10 2000", "2000 11", "1999") must noneBeMatching(FR_FULL)
      (FR_FULL findAllIn        "test2012ok")       must be empty;
      (FR_FULL findAllIn        "test03-2012ok")    must be empty;
    }


    "not match months in any form" in {
      List(
        "janv", "fev", "fév", "févr", "fevr", "avr", "juil", "juill",
        "sept", "oct", "nov", "dec", "déc",
        "janv.", "fev.", "fév.", "févr.", "fevr.", "avr.", "juil.",  "juill.",
        "sept.", "oct.", "nov.", "dec.", "déc.",
        "janvier", "fevrier", "février", "mars",      "avril",   "mai",      "juin",
        "juillet", "aout",    "août",    "septembre", "octobre", "novembre", "decembre", "décembre"
      ) must noneBeMatching(FR_FULL)
    }

    "not match [unseparated] month + YY[YY]" in {
      List(
        "nov2012",      "nov12",        "nov'12",
        "nov.2012",     "nov.12",       "nov.'12",
        "novembre2012", "novembre12",   "novembre'12",
        "nov 2012",      "nov 12",      "nov '12",
        "nov. 2012",     "nov. 12",     "nov. '12",
        "novembre 2012", "novembre 12", "novembre '12"
      ) must noneBeMatching(FR_FULL)
    }
    "not match [unseparated] D[D] + month" in {
      List(
         "3nov",  "3nov.",  "3novembre",
        "03nov", "03nov.", "03novembre",
         "3 nov",  "3 nov.",  "3 novembre",
        "03 nov", "03 nov.", "03 novembre"
      ) must noneBeMatching(FR_FULL)
    }

    "still match [unseparated] D[D] + month + YY[YY]" in {
      List(
        "3nov2012",  "3nov12",  "3nov'12",  "3nov.2012",  "3nov.12",  "3nov.'12",
        "03nov2012",  "03nov12", "03nov'12", "03nov.2012", "03nov.12", "03nov.'12",
         "3novembre2012",  "3novembre12",  "3novembre'12",
        "03novembre2012", "03novembre12", "03novembre'12",
         "3 nov 2012",  "3 nov 12",  "3 nov '12",  "3 nov. 2012",  "3 nov. 12",  "3 nov. '12",
        "03 nov 2012", "03 nov 12", "03 nov '12", "03 nov. 2012", "03 nov. 12", "03 nov. '12",
         "3 novembre 2012",  "3 novembre 12",  "3 novembre '12",
        "03 novembre 2012", "03 novembre 12", "03 novembre '12"
      ) must allBeMatching(FR_FULL)
    }

    "still find full date inside a word" in {
      (FR_FULL findAllIn   "re3nov12v3")    must have size(1)
      (FR_FULL findFirstIn "re3nov12v3")    must_== Some("3nov12")
      (FR_FULL findAllIn   "re3nov2012v3")  must have size(1)
      (FR_FULL findFirstIn "re3nov2012v3")  must_== Some("3nov2012")
      (FR_FULL findAllIn   "re03nov2012v3") must have size(1)
      (FR_FULL findFirstIn "re03nov2012v3") must_== Some("03nov2012")
    }
  }


  "English date regex" should {
    import en.Date.{ALL => EN_ALL, ALPHA => EN_ALPHA}

    "not match empty string" in {
      "" must not be matching(EN_ALL)
      "" must not be matching(EN_ALPHA)
    }

    "still match numeric (with MDY > YMD > DMY)" in {
      List("01/10/2000", "1/3/2000", "00-10-1",
        "10 2000", "2000 11", "1999") must allBeMatching(EN_ALL)
      List("1-21-00", "1/21/2000", "01-21-2000")   must allBeMatching(Date.NUMERIC_US)
      List("1-21-00", "1/21/2000", "01-21-2000")   must allBeMatching(EN_ALL)
      (EN_ALL findFirstMatchIn "01/10/2000") must haveGroup("n_mdy", "01/10/2000")
      (EN_ALL findFirstMatchIn "11-11-2011") must haveGroup("n_mdy", "11-11-2011")
      (EN_ALL findFirstMatchIn "22-11-2011") must haveGroup("n_dmy", "22-11-2011")
      (EN_ALL findFirstMatchIn "1-1-11")     must haveGroup("n_mdy", "1-1-11")
      (EN_ALL findFirstMatchIn "22-1-11")    must haveGroup("n_ymd", "22-1-11")
      (EN_ALL findFirstMatchIn "1-3-2011")   must haveGroup("n_mdy", "1-3-2011")
      (EN_ALL findFirstMatchIn "22-1-2011")  must haveGroup("n_dmy", "22-1-2011")

      (EN_ALL findAllIn        "test2012ok")       must have size(1)
      (EN_ALL findAllIn        "test01-03-2012ok") must have size(1)
      (EN_ALL findAllIn        "test1-3-2012ok")   must have size(1)
      (EN_ALL findAllIn        "test03-2012ok")    must have size(1)
      (EN_ALL findAllIn        "test1-3-12ok")     must have size(1)
      (EN_ALL findFirstMatchIn "test2012ok")       must haveGroup("n_y", "2012")
      (EN_ALL findFirstMatchIn "test01-03-2012ok") must haveGroup("n_mdy", "01-03-2012")
      (EN_ALL findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_mdy", "1-3-2012")
      (EN_ALL findFirstMatchIn "test21-03-2012ok") must haveGroup("n_dmy", "21-03-2012")
      (EN_ALL findFirstMatchIn "test03-2012ok")    must haveGroup("n_my", "03-2012")
      (EN_ALL findFirstMatchIn "test1-3-12ok")     must haveGroup("n_mdy", "1-3-12")
      (EN_ALL findFirstMatchIn "test21-3-12ok")    must haveGroup("n_ymd", "21-3-12")
      (EN_ALL findFirstMatchIn "test22-1-41ok")    must haveGroup("n_dmy", "22-1-41")
    }

    "match months in short form" in {
      List(
        "jan", "feb", "mar",  "apr", "jun", "jul",
        "aug", "sep", "sept", "oct", "nov", "dec"
      ) must allBeMatching(EN_ALL)
      List(
        "jan.", "feb.", "mar.",  "apr.", "jun.", "jul.",
        "aug.", "sep.", "sept.", "oct.", "nov.", "dec."
      ) must allBeMatching(EN_ALL)
    }
    "match months in full form" in {
      List(
        "january", "february", "march",     "april",   "may",      "june",
        "july",    "august",   "september", "october", "november", "december"
      ) must allBeMatching(EN_ALL)
    }

    "match unseparated month + YY[YY]" in {
      List(
        "nov2012",      "nov,2012",
        "nov12",        "nov,12",
        "nov'12",        "nov,'12",
        "nov.2012",     "nov.12",        "nov.'12",
        "november2012", "november,2012",
        "november12",   "november,12",   "november,'12"
      ) must allBeMatching(EN_ALL)
    }
    "match month YY[YY]" in {
      List(
        "nov 2012",      "nov, 2012",
        //"nov 12",      "nov, 12",
        "nov '12",       "nov, '12",
        "nov. 2012",     "nov., 2012",
        //"nov. 12",     "nov., 12",
        "nov. '12",      "nov., '12",
        "november 2012", "november, 2012",
        //"november 12", "november, 12",
        "november '12",  "november, '12"
      ) must allBeMatching(EN_ALL)
    }
    "match 'month YY[YY].' in one match without the final dot" in {
      "nov 2012." must not be matching(EN_ALL)
      (EN_ALL findAllIn   "nov 2012.") must have size(1)
      (EN_ALL findFirstIn "nov 2012.") must_== Some("nov 2012")
    }

    "match unseparated month + D[D]" in {
      List(
        "nov3",      "nov.3",
        "november3", "nov03",
        "nov.03",    "november03"
      ) must allBeMatching(EN_ALL)
    }
    "match month D[D]" in {
      List(
        "nov 3",      "nov. 3",
        "november 3", "nov 03",
        "nov. 03",    "november 03"
      ) must allBeMatching(EN_ALL)
    }
    "match unseparated D[D] + month" in {
      List(
         "3nov",  "3nov.",  "3november",
        "03nov", "03nov.", "03november"
      ) must allBeMatching(EN_ALL)
    }
    "match D[D] month" in {
      List(
         "3 nov",  "3 nov.",  "3 november",
        "03 nov", "03 nov.", "03 november"
      ) must allBeMatching(EN_ALL)
    }

    "match unseparated month + D[D] + ',' + YY[YY]" in {
      List(
        "nov3,2012",       "nov3,12",       "nov3,'12",
        "nov.3,2012",      "nov.3,12",      "nov.3,'12",
        "november3,2012",  "november3,12",  "november3,'12",
        "nov03,2012",      "nov03,12",      "nov03,'12",
        "nov.03,2012",     "nov.03,12",     "nov.03,'12",
        "november03,2012", "november03,12", "november03,'12"
      ) must allBeMatching(EN_ALL)
    }
    "match month D[D][,] YY[YY]" in {
      List(
        "nov 3 2012",       "nov 3rd 2012",      "nov 3, 2012",      "nov 3rd, 2012",
        "nov 3 12",         "nov 3rd 12",        "nov 3, 12",        "nov 3rd, 12",
        "nov 3 '12",        "nov 3rd '12",       "nov 3, '12",       "nov 3rd, '12",
        "nov. 3 2012",      "nov. 3rd 2012",     "nov. 3, 2012",     "nov. 3rd, 2012",
        "nov. 3 12",        "nov. 3rd 12",       "nov. 3, 12",       "nov. 3rd, 12",
        "nov. 3 '12",       "nov. 3rd '12",      "nov. 3, '12",      "nov. 3rd, '12",
        "november 3 2012",  "november 3rd 2012", "november 3, 2012", "november 3rd, 2012",
        "november 3 12",    "november 3rd 12",   "november 3, 12",   "november 3rd, 12",
        "november 3 '12",   "november 3rd '12",  "november 3, '12",  "november 3rd, '12",
        "nov 03 2012",      "nov 03, 2012",
        "nov 03 12",        "nov 03, 12",
        "nov 03 '12",       "nov 03, '12",
        "nov. 03 2012",     "nov. 03, 2012",
        "nov. 03 12",       "nov. 03, 12",
        "nov. 03 '12",      "nov. 03, '12",
        "november 03 2012", "november 03, 2012",
        "november 03 12",   "november 03, 12",
        "november 03 '12",  "november 03, '12"
      ) must allBeMatching(EN_ALL)
    }
    "match 'month D[D][,] YY[YY].' in one match without the final dot" in {
      "nov 1st, 2012." must not be matching(EN_ALL)
      (EN_ALL findAllIn   "nov 1st, 2012.") must have size(1)
      (EN_ALL findFirstIn "nov 1st, 2012.") must_== Some("nov 1st, 2012")
      "nov 3, 2012."   must not be matching(EN_ALL)
      (EN_ALL findAllIn   "nov 3, 2012.")   must have size(1)
      (EN_ALL findFirstIn "nov 3, 2012.")   must_== Some("nov 3, 2012")
      "nov 03 2012."   must not be matching(EN_ALL)
      (EN_ALL findAllIn   "nov 03 2012.")   must have size(1)
      (EN_ALL findFirstIn "nov 03 2012.")   must_== Some("nov 03 2012")
    }

    "match unseparated month + D[D] + ',' + YY[YY]" in {
      List(
        "3nov,2012",       "3nov,12",       "3nov,'12",
        "3nov.,2012",      "3nov.,12",      "3nov.,'12",
        "3november,2012",  "3november,12",  "3november,'12",
        "03nov,2012",      "03nov,12",      "03nov,'12",
        "03nov.,2012",     "03nov.,12",     "03nov.,'12",
        "03november,2012", "03november,12", "03november,'12"
      ) must allBeMatching(EN_ALL)
    }
    "match D[D] month[,] YY[YY]" in {
      List(
         "3 nov 2012",       "3rd nov 2012",      "3 nov, 2012",      "3rd nov, 2012",
         "3 nov 12",         "3rd nov 12",        "3 nov, 12",        "3rd nov, 12",
         "3 nov '12",        "3rd nov '12",       "3 nov, '12",       "3rd nov, '12",
         "3 nov. 2012",      "3rd nov. 2012",     "3 nov., 2012",     "3rd nov., 2012",
         "3 nov. 12",        "3rd nov. 12",       "3 nov., 12",       "3rd nov., 12",
         "3 nov. '12",       "3rd nov. '12",      "3 nov., '12",      "3rd nov., '12",
         "3 november 2012",  "3rd november 2012", "3 november, 2012", "3rd november, 2012",
         "3 november 12",    "3rd november 12",   "3 november, 12",   "3rd november, 12",
         "3 november '12",   "3rd november '12",  "3 november, '12",  "3rd november, '12",
        "03 nov 2012",      "03 nov, 2012",
        "03 nov 12",        "03 nov, 12",
        "03 nov '12",       "03 nov, '12",
        "03 nov. 2012",     "03 nov., 2012",
        "03 nov. 12",       "03 nov., 12",
        "03 nov. '12",      "03 nov., '12",
        "03 november 2012", "03 november, 2012",
        "03 november 12",   "03 november, 12",
        "03 november '12",  "03 november, '12"
      ) must allBeMatching(EN_ALL)
    }
    "match D[D] [of ]month[,] YY[YY]" in {
      List(
         "3 of nov 2012",       "3rd of nov 2012",      "3 of nov, 2012",      "3rd of nov, 2012",
         "3 of nov 12",         "3rd of nov 12",        "3 of nov, 12",        "3rd of nov, 12",
         "3 of nov '12",        "3rd of nov '12",       "3 of nov, '12",       "3rd of nov, '12",
         "3 of nov. 2012",      "3rd of nov. 2012",     "3 of nov., 2012",     "3rd of nov., 2012",
         "3 of nov. 12",        "3rd of nov. 12",       "3 of nov., 12",       "3rd of nov., 12",
         "3 of nov. '12",       "3rd of nov. '12",      "3 of nov., '12",      "3rd of nov., '12",
         "3 of november 2012",  "3rd of november 2012", "3 of november, 2012", "3rd of november, 2012",
         "3 of november 12",    "3rd of november 12",   "3 of november, 12",   "3rd of november, 12",
         "3 of november '12",   "3rd of november '12",  "3 of november, '12",  "3rd of november, '12",
        "03 of nov 2012",      "03 of nov, 2012",
        "03 of nov 12",        "03 of nov, 12",
        "03 of nov '12",       "03 of nov, '12",
        "03 of nov. 2012",     "03 of nov., 2012",
        "03 of nov. 12",       "03 of nov., 12",
        "03 of nov. '12",      "03 of nov., '12",
        "03 of november 2012", "03 of november, 2012",
        "03 of november 12",   "03 of november, 12",
        "03 of november '12",  "03 of november, '12"
      ) must allBeMatching(EN_ALL)
    }

    "not find month inside a word" in {
      (EN_ALL findAllIn   "")            must have size(0)
      (EN_ALL findAllIn   "innovating")  must have size(0)
      (EN_ALL findAllIn   "novice")      must have size(0)
      (EN_ALL findAllIn   "renov")       must have size(0)
      (EN_ALL findAllIn   "innov.ating") must have size(0)
      (EN_ALL findAllIn   "renov.")      must have size(0)
      (EN_ALL findFirstIn "renov2012")   must_== Some("2012")
    }
    "find full date inside a word" in {
      (EN_ALL findAllIn   "re3nov12v3")    must have size(1)
      (EN_ALL findFirstIn "re3nov12v3")    must_== Some("3nov12")
      (EN_ALL findAllIn   "re3nov2012v3")  must have size(1)
      (EN_ALL findFirstIn "re3nov2012v3")  must_== Some("3nov2012")
      (EN_ALL findAllIn   "re03nov2012v3") must have size(1)
      (EN_ALL findFirstIn "re03nov2012v3") must_== Some("03nov2012")
    }
  }

  "English full date regex" should {
    import en.Date.{ALL_FULL => EN_FULL}

    "still match numeric (with MDY > YMD > DMY)" in {
      List("01/10/2000", "00-10-1", "01 10 2000", "1 10 2000") must allBeMatching(EN_FULL)
      List("1-21-00", "01-21-2000", "1-21-2000")               must allBeMatching(EN_FULL)
      (EN_FULL findFirstMatchIn "01/10/2000") must haveGroup("n_mdy", "01/10/2000")
      (EN_FULL findFirstMatchIn "1/10/2000")  must haveGroup("n_mdy", "1/10/2000")
      (EN_FULL findFirstMatchIn "11-11-2011") must haveGroup("n_mdy", "11-11-2011")
      (EN_FULL findFirstMatchIn "22-11-2011") must haveGroup("n_dmy", "22-11-2011")
      (EN_FULL findFirstMatchIn "22-1-2011")  must haveGroup("n_dmy", "22-1-2011")
      (EN_FULL findFirstMatchIn "1-1-11")     must haveGroup("n_mdy", "1-1-11")
      (EN_FULL findFirstMatchIn "1-1-2011")   must haveGroup("n_mdy", "1-1-2011")
      (EN_FULL findFirstMatchIn "22-1-11")    must haveGroup("n_ymd", "22-1-11")

      (EN_FULL findAllIn        "test01-03-2012ok") must have size(1)
      (EN_FULL findAllIn        "test1-3-12ok")     must have size(1)
      (EN_FULL findAllIn        "test1-3-2012ok")   must have size(1)
      (EN_FULL findFirstMatchIn "test01-03-2012ok") must haveGroup("n_mdy", "01-03-2012")
      (EN_FULL findFirstMatchIn "test21-03-2012ok") must haveGroup("n_dmy", "21-03-2012")
      (EN_FULL findFirstMatchIn "test1-3-12ok")     must haveGroup("n_mdy", "1-3-12")
      (EN_FULL findFirstMatchIn "test21-3-12ok")    must haveGroup("n_ymd", "21-3-12")
      (EN_FULL findFirstMatchIn "test22-1-41ok")    must haveGroup("n_dmy", "22-1-41")
      (EN_FULL findFirstMatchIn "test1-3-2012ok")   must haveGroup("n_mdy", "1-3-2012")
      (EN_FULL findFirstMatchIn "test21-3-2012ok")  must haveGroup("n_dmy", "21-3-2012")
    }
    "still not match partial date" in {
      List("10 2000", "2000 11", "1999") must noneBeMatching(EN_FULL)
      (EN_FULL findAllIn        "test2012ok")       must be empty;
      (EN_FULL findAllIn        "test03-2012ok")    must be empty;
    }


    "not match months in any form" in {
      List(
        "jan", "feb", "mar",  "apr", "jun", "jul",
        "aug", "sep", "sept", "oct", "nov", "dec",
        "jan.", "feb.", "mar.",  "apr.", "jun.", "jul.",
        "aug.", "sep.", "sept.", "oct.", "nov.", "dec.",
        "january", "february", "march",     "april",   "may",      "june",
        "july",    "august",   "september", "october", "november", "december"
      ) must noneBeMatching(EN_FULL)
    }

    "not match [unseparated] month + YY[YY][.]" in {
      List(
        "nov2012",       "nov,2012",
        "nov12",         "nov,12",
        "nov.2012",      "nov.12",
        "november2012",  "november,2012",
        "november12",    "november,12",
        "november'12",   "november,'12",
        "nov 2012",      "nov, 2012",
        //"nov 12",      "nov, 12",
        "nov '12",       "nov, '12",
        "nov. 2012",     "nov., 2012",
        //"nov. 12",     "nov., 12",
        "nov. '12",      "nov., '12",
        "november 2012", "november, 2012",
        //"november 12", "november, 12",
        "november '12",  "november, '12"
      ) must noneBeMatching(EN_FULL)

      "nov 2012." must not be matching(EN_FULL)
      (EN_FULL findAllIn "nov 2012.") must be empty;
      (EN_FULL findAllIn "nov 2012.") must be empty;
    }

    "not match [unseparated] month + D[D]" in {
      List(
        "nov3",      "nov.3",
        "november3", "nov03",
        "nov.03",    "november03",
        "nov 3",      "nov. 3",
        "november 3", "nov 03",
        "nov. 03",    "november 03"
      ) must noneBeMatching(EN_FULL)
    }
    "not match [unseparated] D[D] [+ 'of'] + month" in {
      List(
         "3nov",      "3nov.",      "3november",
        "03nov",     "03nov.",     "03november",
         "3 nov",     "3 nov.",     "3 november",
        "03 nov",    "03 nov.",    "03 november",
         "3 of nov",  "3 of nov.",  "3 of november",
        "03 of nov", "03 of nov.", "03 of november"
      ) must noneBeMatching(EN_FULL)
    }

    "still match [unseparated] month D[D][,] YY[YY][.] in one match without the final dot" in {
      List(
        "nov3,2012",       "nov3,12",       "nov3,'12",
        "nov.3,2012",      "nov.3,12",      "nov.3,'12",
        "november3,2012",  "november3,12",  "november3,'12",
        "nov03,2012",      "nov03,12",      "nov03,'12",
        "nov.03,2012",     "nov.03,12",     "nov.03,'12",
        "november03,2012", "november03,12", "november03,'12",
        "nov 3 2012",       "nov 3rd 2012",      "nov 3, 2012",      "nov 3rd, 2012",
        "nov 3 12",         "nov 3rd 12",        "nov 3, 12",        "nov 3rd, 12",
        "nov 3 '12",        "nov 3rd '12",       "nov 3, '12",       "nov 3rd, '12",
        "nov. 3 2012",      "nov. 3rd 2012",     "nov. 3, 2012",     "nov. 3rd, 2012",
        "nov. 3 12",        "nov. 3rd 12",       "nov. 3, 12",       "nov. 3rd, 12",
        "nov. 3 '12",       "nov. 3rd '12",      "nov. 3, '12",      "nov. 3rd, '12",
        "november 3 2012",  "november 3rd 2012", "november 3, 2012", "november 3rd, 2012",
        "november 3 12",    "november 3rd 12",   "november 3, 12",   "november 3rd, 12",
        "november 3 '12",   "november 3rd '12",  "november 3, '12",  "november 3rd, '12",
        "nov 03 2012",      "nov 03, 2012",
        "nov 03 12",        "nov 03, 12",
        "nov 03 '12",       "nov 03, '12",
        "nov. 03 2012",     "nov. 03, 2012",
        "nov. 03 12",       "nov. 03, 12",
        "nov. 03 '12",      "nov. 03, '12",
        "november 03 2012", "november 03, 2012",
        "november 03 12",   "november 03, 12",
        "november 03 '12",  "november 03, '12"
      ) must allBeMatching(EN_FULL)
      "nov 1st, 2012." must not be matching(EN_FULL)
      (EN_FULL findAllIn   "nov 1st, 2012.") must have size(1)
      (EN_FULL findFirstIn "nov 1st, 2012.") must_== Some("nov 1st, 2012")
      "nov 3, 2012."   must not be matching(EN_FULL)
      (EN_FULL findAllIn   "nov 3, 2012.")   must have size(1)
      (EN_FULL findFirstIn "nov 3, 2012.")   must_== Some("nov 3, 2012")
      "nov 03 2012."   must not be matching(EN_FULL)
      (EN_FULL findAllIn   "nov 03 2012.")   must have size(1)
      (EN_FULL findFirstIn "nov 03 2012.")   must_== Some("nov 03 2012")
    }

    "still match [unseparated] D[D] [+ 'of'] + month [+ ','] + YY[YY]" in {
      List(
        "3nov,2012",       "3nov,12",       "3nov,'12",
        "3nov.,2012",      "3nov.,12",      "3nov.,'12",
        "3november,2012",  "3november,12",  "3november,'12",
        "03nov,2012",      "03nov,12",      "03nov,'12",
        "03nov.,2012",     "03nov.,12",     "03nov.,'12",
        "03november,2012", "03november,12", "03november,'12",
        "3 nov 2012",       "3rd nov 2012",      "3 nov, 2012",      "3rd nov, 2012",
        "3 nov 12",         "3rd nov 12",        "3 nov, 12",        "3rd nov, 12",
        "3 nov '12",        "3rd nov '12",       "3 nov, '12",       "3rd nov, '12",
        "3 nov. 2012",      "3rd nov. 2012",     "3 nov., 2012",     "3rd nov., 2012",
        "3 nov. 12",        "3rd nov. 12",       "3 nov., 12",       "3rd nov., 12",
        "3 nov. '12",       "3rd nov. '12",      "3 nov., '12",      "3rd nov., '12",
        "3 november 2012",  "3rd november 2012", "3 november, 2012", "3rd november, 2012",
        "3 november 12",    "3rd november 12",   "3 november, 12",   "3rd november, 12",
        "3 november '12",   "3rd november '12",  "3 november, '12",  "3rd november, '12",
        "03 nov 2012",      "03 nov, 2012",
        "03 nov 12",        "03 nov, 12",
        "03 nov '12",       "03 nov, '12",
        "03 nov. 2012",     "03 nov., 2012",
        "03 nov. 12",       "03 nov., 12",
        "03 nov. '12",      "03 nov., '12",
        "03 november 2012", "03 november, 2012",
        "03 november 12",   "03 november, 12",
        "03 november '12",  "03 november, '12",
        // of
        "3 of nov 2012",       "3rd of nov 2012",      "3 of nov, 2012",      "3rd of nov, 2012",
        "3 of nov 12",         "3rd of nov 12",        "3 of nov, 12",        "3rd of nov, 12",
        "3 of nov '12",        "3rd of nov '12",       "3 of nov, '12",       "3rd of nov, '12",
        "3 of nov. 2012",      "3rd of nov. 2012",     "3 of nov., 2012",     "3rd of nov., 2012",
        "3 of nov. 12",        "3rd of nov. 12",       "3 of nov., 12",       "3rd of nov., 12",
        "3 of nov. '12",       "3rd of nov. '12",      "3 of nov., '12",      "3rd of nov., '12",
        "3 of november 2012",  "3rd of november 2012", "3 of november, 2012", "3rd of november, 2012",
        "3 of november 12",    "3rd of november 12",   "3 of november, 12",   "3rd of november, 12",
        "3 of november '12",   "3rd of november '12",  "3 of november, '12",  "3rd of november, '12",
        "03 of nov 2012",      "03 of nov, 2012",
        "03 of nov 12",        "03 of nov, 12",
        "03 of nov '12",       "03 of nov, '12",
        "03 of nov. 2012",     "03 of nov., 2012",
        "03 of nov. 12",       "03 of nov., 12",
        "03 of nov. '12",      "03 of nov., '12",
        "03 of november 2012", "03 of november, 2012",
        "03 of november 12",   "03 of november, 12",
        "03 of november '12",  "03 of november, '12"
      ) must allBeMatching(EN_FULL)
    }



    "still find full date inside a word" in {
      (EN_FULL findAllIn   "re3nov12v3")    must have size(1)
      (EN_FULL findFirstIn "re3nov12v3")    must_== Some("3nov12")
      (EN_FULL findAllIn   "re3nov2012v3")  must have size(1)
      (EN_FULL findFirstIn "re3nov2012v3")  must_== Some("3nov2012")
      (EN_FULL findAllIn   "re03nov2012v3") must have size(1)
      (EN_FULL findFirstIn "re03nov2012v3") must_== Some("03nov2012")
    }
  }

}