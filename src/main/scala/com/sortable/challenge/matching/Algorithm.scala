package com.sortable.challenge.matching

/**
 * Does 'scoring'/filtering of potential product to listing matches
 */
object Algorithm {
  private val maxPriceSDGap = 0.9

  def filterByPrice(matches: List[MatchComputations]): List[MatchComputations] =
    ReconciliationUtils.filterByPriceGap(matches, maxPriceSDGap)
}
