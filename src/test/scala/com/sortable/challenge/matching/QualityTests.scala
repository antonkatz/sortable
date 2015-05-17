package com.sortable.challenge.matching

import com.sortable.challenge.Main
import org.scalatest.{FlatSpec, Matchers}

/**
 * Testing that recall is up to par. These are not good tests for precision.
 *
 * NOTE: some of counts for missed matches have been tallied before the realization that some listings are identical.
 * The messages have not been modified to preserve previous results.
 */
class QualityTests extends FlatSpec with Matchers {
  private val productPath: String = "data/products.txt"
  private val listingPath: String = "data/listings.txt"
  private val data = Main.loadDataFromFiles(productPath, listingPath)

  "Panasonic Lumix DMC-FH1" should "have 11 (-1) matches" in {
    val fh1 = data flatMap { _._1 find (p => p.name == "Panasonic_Lumix_DMC-FH1") }
    val matches = fh1 map { p => Algorithm.findMatches(p, data.get._2) }

    // one filtered on price
    matches.get should have size (11 - 1)
  }

  "Panasonic FH5" should "have 3 (-1) matches" in {
    val fh5 = data flatMap { _._1 find (p => p.name == "Panasonic-FH5") }
    val matches = fh5 map { p => Algorithm.findMatches(p, data.get._2) }

    // one filtered on price
    matches.get should have size (3 - 1)
  }

  "Fujifilm FinePix F70EXR" should "have 18 (-1) matches" in {
    val f70 = data flatMap { _._1 find (p => p.name == "Fujifilm_FinePix_F70EXR") }
    val matches = f70 map { p => Algorithm.findMatches(p, data.get._2) }

    // one filtered on price
    matches.get should have size (18 - 1)
  }

  "Olympus-TG310" should "have 70 (-2) matches" in {
    val tg310 = data flatMap { _._1 find (p => p.name == "Olympus-TG310") }
    val matches = tg310 map { p => Algorithm.findMatches(p, data.get._2) }

    // 2 listings are fairly poor
    matches.get should have size (70 - 2)
  }

  "Kyocera Yashica Finecam 3300" should "have no matches" in {
    val kyo = data flatMap { _._1 find (p => p.name == "Kyocera_Yashica_Finecam_3300") }
    val matches = kyo map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get shouldBe empty
  }

  "Canon Digital IXUS 100 IS" should "have 15 matches" in {
    val ixus100 = data flatMap { _._1 find (p => p.name == "Canon_Digital_IXUS_100_IS") }
    val matches = ixus100 map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get should have size 15
  }

  "Fujifilm XP30" should "have 52 matches" in {
    val xp30 = data flatMap { _._1 find (p => p.name == "Fujifilm-XP30") }
    val matches = xp30 map { p => Algorithm.findMatches(p, data.get._2) }

    // most likely wrong currency specified in one of the listings. Algorithm should account for such cases.
    matches.get should have size 52
  }

  "Casio TRYX" should "have 4 (-1) matches" in {
    val tryx = data flatMap { _._1 find (p => p.name == "Casio-TRYX") }
    val matches = tryx map { p => Algorithm.findMatches(p, data.get._2) }

    // most likely wrong currency specified in one of the listings. Algorithm should account for such cases.
    matches.get should have size (4 - 1)
  }

  "Kodak Slice" should "have 35 matches" in {
    val slice = data flatMap { _._1 find (p => p.name == "Kodak-slice") }
    val matches = slice map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get should have size 35
  }

  "Sony Alpha NEX-5" should "have 80 (-4) matches" in {
    // listings with letters at the end (ex. nex-5k) are referring to the same model
    val nex5 = data flatMap { _._1 find (p => p.name == "Sony_Alpha_NEX-5") }
    val matches = nex5 map { p => Algorithm.findMatches(p, data.get._2) }

    // all are lost on having too many model matches
    matches.get should have size 88
  }

  "Casio Exilim EX-FH25" should "have 4 (-1) matches" in {
    val fh25 = data flatMap { _._1 find (p => p.name == "Casio_Exilim_EX-FH25") }
    val matches = fh25 map { p => Algorithm.findMatches(p, data.get._2) }

    // filtered by price
    matches.get should have size (4 - 1)
  }

  "Pentax K-R" should "have 63 (-1) matches" in {
    val kr = data flatMap { _._1 find (p => p.name == "Pentax_K-r") }
    val matches = kr map { p => Algorithm.findMatches(p, data.get._2) }

    // filtered by having too many random model matches
    matches.get should have size 65
  }

  "Pentax K-X" should "have 104 (-2) matches" in {
    val kx = data flatMap { _._1 find (p => p.name == "Pentax_K-x") }
    val matches = kx map { p => Algorithm.findMatches(p, data.get._2) }

    // 2 by model being too far from manufacturer
    matches.get should have size (104 - 2)
  }

  "Sony S930" should "have 14 matches" in {
    val s930 = data flatMap { _._1 find (_.model == "DSC-S930") }
    val matches = s930 map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get should have size 14
  }

  "Olympus Tough 6000" should "have 28 matches" in {
    val t6000 = data flatMap { _._1 find (_.model == "Tough 6000") }
    val matches = t6000 map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get should have size 29
  }

  "Olympus Stylus 7010" should "have 22 (-1) matches" in {
    val s7010 = data flatMap { _._1 find (p => p.model == "7010" && p.manufacturer == "Olympus") }
    val matches = s7010 map { p => Algorithm.findMatches(p, data.get._2) }

    // one is filtered out by price. ridiculously overpriced.
    matches.get should have size (22 - 1)
  }

  "Nikon S640" should "have 13 matches" in {
    val s640 = data flatMap { _._1 find (p => p.model == "S640" && p.manufacturer == "Nikon") }
    val matches = s640 map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get should have size 13
  }

  "Sanyo DSC-SX1Z" should "have no matches" in {
    val sx1z = data flatMap { _._1 find (p => p.model == "DSC-SX1Z" && p.manufacturer == "Sanyo") }
    val matches = sx1z map { p => Algorithm.findMatches(p, data.get._2) }

    // couldn't find any matches by hand
    matches.get shouldBe empty
  }

  "Ricoh CX2" should "have 19 matches" in {
    val cx2 = data flatMap { _._1 find (p => p.model == "CX2" && p.manufacturer == "Ricoh") }
    val matches = cx2 map { p => Algorithm.findMatches(p, data.get._2) }

    matches.get should have size 19
  }

  "Contax N Digital" should "have no matches" in {
    val nDigital = data flatMap { _._1 find (p => p.name == "Contax_N_Digital") }
    val matches = nDigital map { p => Algorithm.findMatches(p, data.get._2) }

    // found none by hand
    matches.get shouldBe empty
  }
}
