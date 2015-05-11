package com.sortable.challenge

import org.scalatest.{Matchers, FlatSpec}
import com.sortable.challenge.matching.ReconciliationUtils._
/**
 * Testing that recall is up to par.
 */
class Recall extends FlatSpec with Matchers {
  val productPath: String = "data/products.txt"
  val listingPath: String = "data/listings.txt"
  val data = Main.loadDataFromFiles(productPath, listingPath)

  "Sony S930" should "have 14 matches" in {
    val s930 = data flatMap {_._1 find(_.model == "DSC-S930")}
    val matches = s930 map {p => findConcreteMatches(p, data.get._2)}

    matches.get should have size 14
  }

  "Olympus Tough 6000" should "have 28 matches" in {
    val t6000 = data flatMap {_._1 find(_.model == "Tough 6000")}
    val matches = t6000 map {p => findConcreteMatches(p, data.get._2)}

    matches.get should have size 28
  }

  "Olympus Stylus 7010" should "have 22 matches" in {
    val s7010 = data flatMap {_._1 find(p => p.model == "7010" && p.manufacturer == "Olympus")}
    val matches = s7010 map {p => findConcreteMatches(p, data.get._2)}

    // one is filtered out by price. ridiculously overpriced.
    matches.get should have size (22 - 1)
  }

  "Nikon S640" should "have _ matches" in {
    val s640 = data flatMap {_._1 find(p => p.model == "S640" && p.manufacturer == "Nikon")}
    val matches = s640 map {p => findConcreteMatches(p, data.get._2)}

    matches.get should have size 13
  }
}
