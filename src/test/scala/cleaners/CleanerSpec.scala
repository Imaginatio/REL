package fr.splayce.REL.cleaners

import org.specs2.mutable._

class CleanerSpec extends Specification {

  "Identity Cleaner" should {
    "leave string untouched" in {
      val dirty = " dirty  ''.+string"
      IdentityCleaner(dirty) must_== dirty
    }
  }

  "Diacritic Cleaner" should {
    "remove diacritics" in {
      DiacriticCleaner("àäâéèêëïîôöùüûç") must_== "aaaeeeeiioouuuc"
    }
  }

  "CamelCase Splitter" should {
    "split CamelCase words" in {
      CamelCaseSplitter("someWords in CamelCase") must_== "some Words in Camel Case"
    }
    "ignore double uppercase" in {
      CamelCaseSplitter("NOtCAmelCAse") must_== "NOtCAmelCAse"
    }
    "ignore trailing uppercase" in {
      CamelCaseSplitter("TestT") must_== "TestT"
    }
  }

  "Cleaner Chain" should {
    class PrefixingCleaner(val prefix: String) extends Cleaner {
      override def clean(in: String) = prefix + in
    }
    
    val aPrefix = new PrefixingCleaner("a ")
    val bPrefix = new PrefixingCleaner("b ")
    val cPrefix = new PrefixingCleaner("c ")
    val chain = aPrefix(bPrefix(cPrefix))

    "perform all cleaners in order" in {
      chain("test") must_== "a b c test"
    }
  }

}