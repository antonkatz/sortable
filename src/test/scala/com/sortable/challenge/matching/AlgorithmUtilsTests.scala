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
    val result = producePermutationsFromBins(testBins)

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
    var prices = Seq(5, 6, 90, 100, 110, 105, 107, 111, 113, 120, 150, 400)
    var matches = prices map { p => new Listing("", "", "CAD", p) } map { l => new PairHolder(product, l) }
    var filtered = filterByPriceGap(matches.toList, 0.8)

    assert(!filtered.exists(_.listing.price == 6))
    assert(!filtered.exists(_.listing.price == 5))
    assert(filtered exists (_.listing.price == 90))
    assert(filtered exists (_.listing.price == 150))
    assert(!filtered.exists(_.listing.price == 400))

    prices = Seq(100, 125, 85, 150, 135, 140, 151, 185, 120, 200, 130, 200, 250)
    val currency = Seq("USD", "USD", "GBP", "USD", "EUR", "EUR", "EUR", "USD", "GBP", "USD", "GBP", "EUR", "EUR")
    assert(prices.length == currency.length)
    matches = prices.zip(currency) map { p => new Listing("", "", p._2, p._1) } map { l => new PairHolder(product, l) }
    filtered = filterByPriceGap(matches.toList, 0.9)

    assert(!filtered.exists(_.listing.price == 250))
    filtered should have size 12
  }

  "Simple similarity of two strings" should "be calculated correctly" in {
    simpleStringSimilarity("Abc", "addr").get should equal(0.33 +- 0.01)
    simpleStringSimilarity("abc", "aaddr").get should equal(0.33 +- 0.01)
    simpleStringSimilarity("Abbc", "aabr").get should equal(0.5 +- 0.01)
    simpleStringSimilarity("Contax", "sony").get should equal(0.33 +- 0.01)
  }

  "Impure model modifiers" should "be detected correctly" in {
    val set1 = Set((_t, 0, 0, "mod"), (_t, 0, 5, "10"), (_t, 3, 5, "10"))
    val set2 = Set((_t, 0, 0, "4"), (_t, 0, 5, "mod"), (_t, 0, 11, "10"))
    val set3 = Set((_t, 0, 2, "e"), (_t, 0, 6, "mod"), (_t, 0, 11, "10"))

    getImpureNumericPhraseCount(set1, "modaa10a") shouldBe 0
    getImpureNumericPhraseCount(set1, "moda4a10") shouldBe 1
    getImpureNumericPhraseCount(set2, "4a9aamoda7a10") shouldBe 2
    getImpureNumericPhraseCount(set3, "56eaaamod8s10") shouldBe 1
  }
}
