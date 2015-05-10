package com.sortable.challenge.matching

import org.scalatest.{Matchers, FlatSpec}

/**
 * Miscellaneous tests for disparate methods to flush out bugs.
 */
class Misc extends FlatSpec with Matchers {
	/** Used where any type will do. */
	val _t = TokenMatchType.nameToTitle

	"Several bins (list of lists)" should "be correctly combined" in {
		val testBins = List(List(1, 2), List(2, 3), List(4, 5))
		val result = MatchingUtils.produceCombinationsFromBins(testBins)

		result should contain allOf (List(1,2,4), List(1,3,4), List(2,2,5))
		result.size should equal (result.toSet.size)
	}

	"Token matches" should "be correctly clustered" in {
		val testTokenMatches = Iterable((_t, 0, 0), (_t, 0, 10), (_t, 2, 1), (_t, 2, 15), (_t, 3, 5), (_t, 6, 7))
		MatchingUtils.findTightestCluster(testTokenMatches) should contain noneOf((_t, 0, 10), (_t, 2, 15))
	}

	"Displacement of token matches" should "be correctly calculated" in {
		val testTokenMatches = Seq((_t, 0, 14), (_t, 10, 5), (_t, 20, 15), (_t, 30, 13))
		val result = MatchingUtils.getOrderChangeAroundPivot(testTokenMatches)

		result should contain allOf ((_t, 0, 14) -> 0, (_t, 10, 5) -> -3, (_t, 20, 15) -> -1, (_t, 30, 13) -> -4)
	}
}
