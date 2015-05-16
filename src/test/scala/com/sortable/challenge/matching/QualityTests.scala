package com.sortable.challenge.matching

import com.sortable.challenge.Main
import org.scalatest.{FlatSpec, Matchers}

/**
 * Testing that recall is up to par. These are not good tests for precision.
 */
class QualityTests extends FlatSpec with Matchers {
  val productPath: String = "data/products.txt"
  val listingPath: String = "data/listings.txt"
  val data = Main.loadDataFromFiles(productPath, listingPath)

  "Sony Alpha NEX-5" should "have 63 matches" in {
    // listings with letters at the end (ex. nex-5k) are referring to the same model
    val nex5 = data flatMap {_._1 find(p => p.name == "Sony_Alpha_NEX-5")}
    val matches = nex5 map {p => Algorithm.findMatches(p, data.get._2)}

    matches.get should have size (4-1)
  }

  "Casio Exilim EX-FH25" should "have 4 (-1) matches" in {
    val fh25 = data flatMap {_._1 find(p => p.name == "Casio_Exilim_EX-FH25")}
    val matches = fh25 map {p => Algorithm.findMatches(p, data.get._2)}

    // filtered by price
    matches.get should have size (4-1)
  }

  "Pentax K-R" should "have 63 matches" in {
    val kr = data flatMap {_._1 find(p => p.name == "Pentax_K-r")}
    val matches = kr map {p => Algorithm.findMatches(p, data.get._2)}

    // filtered by having too many random model matches
    matches.get should have size 63
  }

  "Pentax K-X" should "have 97 (-1 + -2) matches" in {
    val kx = data flatMap {_._1 find(p => p.name == "Pentax_K-x")}
    val matches = kx map {p => Algorithm.findMatches(p, data.get._2)}

    // 1 filtered by price, 2 by model being too far from manufacturer
    matches.get should have size (97 - 3)
  }

  "Sony S930" should "have 14 matches" in {
    val s930 = data flatMap {_._1 find(_.model == "DSC-S930")}
    val matches = s930 map {p => Algorithm.findMatches(p, data.get._2)}

    matches.get should have size 14
  }

  "Olympus Tough 6000" should "have 28 matches" in {
    val t6000 = data flatMap {_._1 find(_.model == "Tough 6000")}
    val matches = t6000 map {p => Algorithm.findMatches(p, data.get._2)}

    matches.get should have size 28
  }

  "Olympus Stylus 7010" should "have 22 (-1) matches" in {
    val s7010 = data flatMap {_._1 find(p => p.model == "7010" && p.manufacturer == "Olympus")}
    val matches = s7010 map {p => Algorithm.findMatches(p, data.get._2)}

    // one is filtered out by price. ridiculously overpriced.
    matches.get should have size (22 - 1)
  }

  "Nikon S640" should "have 13 (-1) matches" in {
    val s640 = data flatMap {_._1 find(p => p.model == "S640" && p.manufacturer == "Nikon")}
    val matches = s640 map {p => Algorithm.findMatches(p, data.get._2)}

    // one filtered by price
    // todo flickers sometimes depending on whether accessories are matched (driving SD higher)
    matches.get should have size (13)
  }

  "Sanyo DSC-SX1Z" should "have no matches" in {
    val sx1z = data flatMap {_._1 find(p => p.model == "DSC-SX1Z" && p.manufacturer == "Sanyo")}
    val matches = sx1z map {p => Algorithm.findMatches(p, data.get._2)}

    // couldn't find any matches by hand
    matches.get shouldBe empty
  }

  "Ricoh CX2" should "have 19 matches" in {
    val cx2 = data flatMap {_._1 find(p => p.model == "CX2" && p.manufacturer == "Ricoh")}
    val matches = cx2 map {p => Algorithm.findMatches(p, data.get._2)}

    matches.get should have size 19
  }

  "Contax N Digital" should "have no matches" in {
    val nDigital = data flatMap {_._1 find(p => p.name == "Contax_N_Digital")}
    val matches = nDigital map {p => Algorithm.findMatches(p, data.get._2)}

    // found none by hand
    matches.get shouldBe empty
  }
}
