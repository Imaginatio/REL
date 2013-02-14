package fr.splayce.rel.flavors

import org.specs2.mutable._

import fr.splayce.rel._


object GroupNameSimplifierSpec extends Specification {

  import Symbols._
  import Implicits.string2RE

  "Group Name Simplifier translation" should {

    val g1  = Group("strictName1", RE("a"), Some(ChevNamingStyle))
    val g2  = g1.copy("snake_name_2")
    val g2_ = g1.copy("snakename2")
    val g3  = g1.copy("lenient name #3!")
    val g3_ = g1.copy("lenientname3")
    val g4  = g1.copy("0 crazy name #4!")
    val g4_ = g1.copy("crazyname4")
    val g4l = g1.copy("crazy name #4!")
    val g5  = g1.copy("#!")
    val g5_ = g1.copy("#!", embedStyle = None)
    val gr  = Group("wrapping group", g2, Some(ChevNamingStyle))
    val gr_ = Group("wrappinggroup", g2_, Some(ChevNamingStyle))

    "check preconditions" in {
      g1 .name must     be matching(RE. strictGroupName)
      g1 .name must     be matching(RE.  snakeGroupName)
      g1 .name must     be matching(RE.lenientGroupName)

      g2 .name must not be matching(RE. strictGroupName)
      g2 .name must     be matching(RE.  snakeGroupName)
      g2 .name must     be matching(RE.lenientGroupName)
      g2_.name must     be matching(RE. strictGroupName)
      g2_.name must     be matching(RE.  snakeGroupName)
      g2_.name must     be matching(RE.lenientGroupName)

      g3 .name must not be matching(RE. strictGroupName)
      g3 .name must not be matching(RE.  snakeGroupName)
      g3 .name must     be matching(RE.lenientGroupName)
      g3_.name must     be matching(RE. strictGroupName)
      g3_.name must     be matching(RE.  snakeGroupName)
      g3_.name must     be matching(RE.lenientGroupName)

      g4 .name must not be matching(RE. strictGroupName)
      g4 .name must not be matching(RE.  snakeGroupName)
      g4 .name must not be matching(RE.lenientGroupName)
      g4_.name must     be matching(RE. strictGroupName)
      g4_.name must     be matching(RE.  snakeGroupName)
      g4_.name must     be matching(RE.lenientGroupName)
      g4l.name must not be matching(RE. strictGroupName)
      g4l.name must not be matching(RE.  snakeGroupName)
      g4l.name must     be matching(RE.lenientGroupName)

      g5 .name must not be matching(RE. strictGroupName)
      g5 .name must not be matching(RE.  snakeGroupName)
      g5 .name must not be matching(RE.lenientGroupName)
    }

    "provide strict compatible group names" in {
      val tr = { (re: RE) => GroupNameSimplifier.strict.translate(re) }
      tr(g1) must_== g1
      tr(g2) must_== g2_
      tr(g3) must_== g3_
      tr(g4) must_== g4_
      tr(g5) must_== g5_

      // recursion check
      tr(gr) must_== gr_
    }

    "provide snake compatible group names" in {
      val tr = { (re: RE) => GroupNameSimplifier.snake.translate(re) }
      tr(g1) must_== g1
      tr(g2) must_== g2
      tr(g3) must_== g3_
      tr(g4) must_== g4_
      tr(g5) must_== g5_
    }

    "provide lenient compatible group names" in {
      val tr = { (re: RE) => GroupNameSimplifier.lenient.translate(re) }
      tr(g1) must_== g1
      tr(g2) must_== g2
      tr(g3) must_== g3
      tr(g4) must_== g4l
      tr(g5) must_== g5_
    }

  }

}