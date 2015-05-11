package com.sortable.challenge.matching

import com.sortable.challenge.matching.TokenMatchingUtils._
import com.sortable.challenge.matching.TokenMatchType._
import com.sortable.challenge.{Listing, Product, DataHolder, TokenizationUtils}
import org.scalatest.{Matchers, FlatSpec}

/**
 * Miscellaneous tests for disparate methods to flush out bugs.
 */
class Misc extends FlatSpec with Matchers {
	/** Used where any type will do. */
	val _t = TokenMatchType.nameToTitle

	"Several bins (list of lists)" should "be correctly combined" in {
		val testBins = List(List(1, 2), List(2, 3), List(4, 5))
		val result = TokenMatchingUtils.produceCombinationsFromBins(testBins)

		result should contain allOf (List(1,2,4), List(1,3,4), List(2,2,5))
		result.size should equal (result.toSet.size)
	}

	"Token matches" should "be correctly clustered" in {
		val testTokenMatches = Iterable((_t, 0, 0, ""), (_t, 0, 10, ""), (_t, 2, 1, ""), (_t, 2, 15, ""),
			(_t, 3, 5, ""), (_t, 6, 7, ""))
		TokenMatchingUtils.findTightestCluster(testTokenMatches) should contain noneOf((_t, 0, 10, ""), (_t, 2, 15, ""))
	}

	"Displacement of token matches" should "be correctly calculated" in {
		val testTokenMatches = Seq((_t, 0, 14, ""), (_t, 10, 5, ""), (_t, 20, 15, ""), (_t, 30, 13, ""))
		val result = TokenMatchingUtils.getOrderChangeAroundPivot(testTokenMatches)

		result should contain allOf (
				(_t, 0, 14, "") -> 0, (_t, 10, 5, "") -> -3, (_t, 20, 15, "") -> -1, (_t, 30, 13, "") -> -4)
	}

	"Tokens" should "be split into two tokens if they contain any number of digits, with the correct indexes" in {
		Seq(("test16test5", 10), ("1test", 0), ("test", 0), ("5000t", 0)) flatMap {
			TokenizationUtils.splitTokenWithNumber
		} should contain allOf (
				("test", 10), ("16test5", 14), ("1", 0), ("test", 1), ("test", 0), ("5000", 0), ("t", 4))
	}

	"Tokenization" should "be executed correctly" in {
		Seq("ttest16test5", "1test", "test", "5000t") flatMap {t =>
			TokenizationUtils.tokenizeWithIndex(t, Set(' ', '_', '-', '/'))} should
				contain allOf (("ttest", 0), ("16test5", 5), ("1", 0), ("test", 1), ("test", 0), ("5000", 0), ("t", 4))
	}

	"Average relative positional variance" should "be computed correctly" in {
		TokenMatchingUtils.computeRelativePositionalVariance(
			Iterable((_t, 0, 10, "abc"), (_t, 0, 20, "ab"), (_t, 0, 100, "abcd"))) should equal (540.74 +- 0.01)
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
			var tokens = matchTokens(product.getModelTokens, listingTitle, _t)
			tokens = TokenMatchingUtils.filterOvermatched(tokens)

			assert(tokens exists (t => t._4 == "s" && t._3 == 0))
			assert(tokens exists (t => t._4 == "s" && t._3 == 9))
			assert(!tokens.exists(t => t._4 == "s" && t._3 == 6))
		}
	}

	// better to get a false positive with this one, then a false negative (for example kits)
	"Matches with prices that are obviously outliers" should "be carefully filtered out" in {
		val product = new Product("", "", "", None)
		val prices = Set(10, 11, 90, 100, 110, 105, 107, 120, 140, 145, 150, 400)
		val matches = prices map {p => new Listing("", "", "CAD", p)} map {l => new MatchComputations(product, l)}
		val filtered = AnalysisUtils.filterByPrice(matches.toList)

		assert(!filtered.exists(_.listing.price == 10))
		assert(!filtered.exists(_.listing.price == 20))
		assert(filtered exists(_.listing.price == 100))
		assert(filtered exists(_.listing.price == 150))
		assert(!filtered.exists(_.listing.price == 400))
	}
}
