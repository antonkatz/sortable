package com.sortable.challenge.matching

/**
 * Functions for testing and scoring matches between products and listings.
 * These are somewhat algorithm dependant.
 */
object AlgorithmUtils {
  /** Assuming sample data, rather than the whole population. */
  private def computeSD(iterable: Iterable[Double]): Double = {
    val size = iterable.size
    val avg = iterable.sum / size
    val variance = (iterable map { i => Math.pow(i - avg, 2) } sum) / size
    Math.sqrt(variance)
  }

  private def computeMatchSd(matches: Iterable[PairHolder]): Double = computeSD(getPrices(matches))

  private def getPrices(matches: Iterable[PairHolder]) = matches map { _.listing.adjustedPrice } flatten

  private[matching] def filterByPriceGap(matches: List[PairHolder], maxSdGap: Double): List[PairHolder] =
  {
    val sd = computeMatchSd(matches)
    val sortedByPrice =
      matches map { m => m -> m.listing.adjustedPrice } collect { case (m, Some(p)) => m -> p } sortBy(_._2)
    val sortedPrices = sortedByPrice map (_._2)
    val sortedMatches = sortedByPrice map (_._1)

    sortedPrices match {
      case head :: tail =>
        val neighbourDistances = sortedPrices.tail.foldLeft {(sortedPrices.head, List[Double]())} {
          (f: (Double, List[Double]), p: Double) => (p, f._2 :+ {p - f._1})
        } _2
        var gapPositions:List[Int] =
          neighbourDistances.zipWithIndex collect{case (d, i) if d / sd > maxSdGap => i + 1} sorted;
        gapPositions = 0 +: gapPositions :+ sortedByPrice.length
        val clusters = {gapPositions zip gapPositions.tail} map {case (s,e) => sortedMatches.slice(s, e)}
        clusters.sortBy(_.size) last
      case _ => matches
    }
  }
}
