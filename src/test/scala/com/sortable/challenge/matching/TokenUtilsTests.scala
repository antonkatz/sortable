package com.sortable.challenge.matching

import com.sortable.challenge.matching.TokenMatchingUtils._
import com.sortable.challenge.{Listing, Product, TokenizationUtils}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Tests for disparate methods related to tokenization and token matching; used to flush out bugs.
 * These are NOT meant to be concrete tests.
 */
class TokenUtilsTests extends FlatSpec with Matchers {
  /** Used where any type will do. */
  val _t = TokenMatchType.nameToTitle

  "Displacement of token matches" should "be correctly calculated" in {
    val testTokenMatches = Seq((_t, 0, 14, ""), (_t, 10, 5, ""), (_t, 20, 15, ""), (_t, 30, 13, ""))
    val result = getOrderChangeAroundPivot(testTokenMatches)

    result should contain allOf(
        (_t, 0, 14, "") -> 0, (_t, 10, 5, "") -> -3, (_t, 20, 15, "") -> -1, (_t, 30, 13, "") -> -4)
  }

  "Tokens" should "be split into two tokens if they contain any number of digits, with the correct indexes" in {
    Seq(("test16test5", 10), ("1test", 0), ("test", 0), ("5000t", 0)) flatMap {
      TokenizationUtils.splitTokenWithNumber
    } should contain allOf(
        ("test", 10), ("16test5", 14), ("1", 0), ("test", 1), ("test", 0), ("5000", 0), ("t", 4))
  }

  "Tokenization" should "be executed correctly" in {
    Seq("ttest16test5", "1test", "test", "5000t") flatMap { t =>
      TokenizationUtils.tokenizeWithIndex(t, Set(' ', '_', '-', '/'))
    } should
        contain allOf(("ttest", 0), ("16test5", 5), ("1", 0), ("test", 1), ("test", 0), ("5000", 0), ("t", 4))
  }

  /** Related tests of token matching/filtering. */
  {
    val listingTitle = "sony dsc-s930 digitalkamera (10 megapixel, 3-fach opt. zoom, 6,1 cm (2,4 zoll) display) silber"
    val model = "DSC-S930"
    "All tokens" should "be found in the destination string if present" in {
      val product = new Product("Sony_Cyber-shot_DSC-S930", "Sony", model, Some("Cyber-shot"))
      val tokens = matchTokens(product.getModelTokens, listingTitle, _t)

      tokens should contain allOf((_t, 4, 0, "s"), (_t, 4, 6, "s"), (_t, 4, 9, "s"), (_t, 4, 81, "s"), (_t, 4, 88, "s"),
          (_t, 0, 5, "dsc"), (_t, 5, 10, "930"))
    }

    "'Overmatched' filter" should "be filtering correct tokens" in {
      val product = new Product("Sony_Cyber-shot_DSC-S930", "Sony", model, Some("Cyber-shot"))
      var tokens = matchTokens(product.getModelTokens, listingTitle, _t) ++ Iterable((_t, 0, 16, "DSC"))
      tokens = filterOvermatched(tokens)

      assert(tokens exists (t => t._4 == "s" && t._3 == 0))
      assert(tokens exists (t => t._4 == "s" && t._3 == 9))
      assert(!tokens.exists(t => t._4 == "s" && t._3 == 6))
      assert(tokens.exists(t => t._4 == "DSC"))
    }
  }

  // better to get a false positive with this one, then a false negative (for example kits)
  "Matches with prices that are obviously outliers" should "be carefully filtered out" in {
    val product = new Product("", "", "", None)
    val prices = Set(5, 6, 90, 100, 110, 105, 107, 111, 113, 120, 150, 400)
    val matches = prices map { p => new Listing("", "", "CAD", p) } map { l => new PairHolder(product, l) }
    val filtered = AlgorithmUtils.filterByPriceGap(matches.toList, 0.8)

    assert(!filtered.exists(_.listing.price == 6))
    assert(!filtered.exists(_.listing.price == 5))
    assert(filtered exists (_.listing.price == 90))
    assert(filtered exists (_.listing.price == 150))
    assert(!filtered.exists(_.listing.price == 400))
  }

  "Impure numeric tokens" should "be detected correctly" in {
    val t = TokenMatchType.nameToTitle
    val tokens = Set((t, 0, 10, "a"), (t, 0, 0, "10"), (t, 0, 10, "87"), (t, 0, 20, "55"))
    val listing = new Listing("10aaaaaa687aaaaaaaaa559", "", "CAD", 0.0)
    val result = getImpureNumericMatches(tokens, listing)

    result should contain allOf((t, 0, 10, "87"), (t, 0, 20, "55"))
    result should contain noneOf((t, 0, 10, "a"), (t, 0, 0, "10"))
  }

  "Impure tokens" should "be detected correctly" in {
    val t = TokenMatchType.nameToTitle
    val tokens =
      Set((t, 0, 13, "b"), (t, 0, 15, "c"), (t, 0, 19, "d"), (t, 0, 0, "10"), (t, 0, 9, "87"), (t, 0, 24, "55"))
    val listing = new Listing("10aaaaaa687a b caa d-aaa559", "", "CAD", 0.0)
    val result = getImpureMatches(tokens, listing)

    result should contain allOf((t, 0, 9, "87"), (t, 0, 24, "55"), (t, 0, 15, "c"))
    result should contain noneOf((t, 0, 13, "b"), (t, 0, 0, "10"), (t, 0, 19, "d"))
  }

  "Impure model modifiers" should "be detected correctly" in {
    val set1 = Set((_t, 0, 0, "mod"), (_t, 0, 6, "10"))
    val set2 = Set((_t, 0, 0, "4"), (_t, 0, 5, "mod"), (_t, 0, 11, "10"))

    getImpurePhraseCount(set1, "modaaa10") shouldBe 0
    getImpurePhraseCount(set1, "moda4a10") shouldBe 1
    getImpurePhraseCount(set2, "4a9aamoda7a10") shouldBe 2
  }

  "Closest letters to numbers" should "be found in origin strings" in {
    val set1 = Set(("a", 10), ("10", 0), ("b", 5))
    val set2 = Set(("a", 0), ("10", 10), ("b", 25))
    val set3 = Set(("a", 0), ("10", 10), ("b", 25), ("5", 26))
    val set4 = Set(("a", 9), ("10", 10), ("b", 12))
    val set5 = Set(("a", 0), ("b", 5))
    val set6 = Set(("10", 10))

    getLettersAroundDigits(set1) should contain only (("b", 5))
    getLettersAroundDigits(set2) should contain only (("a", 0))
    getLettersAroundDigits(set3) should contain only (("a", 0), ("b", 25))
    getLettersAroundDigits(set4) should contain only (("a", 9), ("b", 12))
    getLettersAroundDigits(set5) shouldBe empty
    getLettersAroundDigits(set6) shouldBe empty
  }
}