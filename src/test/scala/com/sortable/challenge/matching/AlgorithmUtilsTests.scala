package com.sortable.challenge.matching

import com.sortable.challenge.matching.AlgorithmUtils._
import com.sortable.challenge.{Listing, Product}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Tests for methods that are somewhat algorithm dependant; used to flush out bugs.
 * These are NOT meant to be concrete tests.
 */
class AlgorithmUtilsTests extends FlatSpec with Matchers {
  /** Used where any type will do. */
  val _t = TokenMatchType.nameToTitle

  "Several bins (list of lists)" should "be correctly combined" in {
    val testBins = List(List(1, 2), List(2, 3), List(4, 5))
    val result = produceCombinationsFromBins(testBins)

    result should contain allOf(List(1, 2, 4), List(1, 3, 4), List(2, 2, 5))
    result.size should equal(result.toSet.size)
  }

  "Token matches" should "be correctly clustered" in {
    val testTokenMatches = Iterable((_t, 0, 0, ""), (_t, 0, 10, ""), (_t, 2, 1, ""), (_t, 2, 15, ""),
      (_t, 3, 5, ""), (_t, 6, 7, ""))
    findTightestCluster(testTokenMatches) should contain noneOf((_t, 0, 10, ""), (_t, 2, 15, ""))
  }

  "Average dispersion" should "be computed correctly" in {
    computeAverageDispersion(
      Iterable((_t, 0, 10, "abc"), (_t, 0, 20, "ab"), (_t, 0, 100, "abcd"))) should equal(42.5 +- 0.001)
    computeAverageDispersion(
      Iterable((_t, 0, 4, "k"), (_t, 0, 8, "x"))) should equal(3.0 +- 0.001)
    computeAverageDispersion(Iterable((_t, 0, 4, "k"))) should equal(0.0)
  }

  // better to get a false positive with this one, then a false negative (for example kits)
  "Matches with prices that are obviously outliers" should "be carefully filtered out" in {
    val product = new Product("", "", "", None)
    val prices = Set(5, 6, 90, 100, 110, 105, 107, 111, 113, 120, 150, 400)
    val matches = prices map { p => new Listing("", "", "CAD", p) } map { l => new PairHolder(product, l) }
    val filtered = filterByPriceGap(matches.toList, 0.8)

    assert(!filtered.exists(_.listing.price == 6))
    assert(!filtered.exists(_.listing.price == 5))
    assert(filtered exists (_.listing.price == 90))
    assert(filtered exists (_.listing.price == 150))
    assert(!filtered.exists(_.listing.price == 400))
  }

  "Simple similarity of two strings" should "be calculated correctly" in {
    simpleSimilarity("Abc", "addr").get should equal(0.33 +- 0.01)
    simpleSimilarity("abc", "aaddr").get should equal(0.33 +- 0.01)
    simpleSimilarity("Abbc", "aabr").get should equal(0.5 +- 0.01)
    simpleSimilarity("Contax", "sony").get should equal(0.33 +- 0.01)
  }
}
