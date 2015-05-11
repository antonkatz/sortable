package com.sortable.challenge.matching

import com.sortable.challenge.{Listing, Product}

/**
 * Functions for testing and scoring matches between products and listings.
 * These are somewhat algorithm dependant
 */
object ReconciliationUtils {
  /** Assuming sample data, rather than the whole population. */
  private def computeSD(iterable: Iterable[Double]): Double = {
    val size = iterable.size
    val avg = iterable.sum / size
    val variance = (iterable map { i => Math.pow(i - avg, 2) } sum) / size
    Math.sqrt(variance)
  }

  private def computeMatchSd(matches: Iterable[MatchComputations]): Double = computeSD(getPrices(matches))

  private def getPrices(matches: Iterable[MatchComputations]) = matches map { _.listing.adjustedPrice } flatten

  private[matching] def filterByPriceGap(matches: List[MatchComputations], maxSdGap: Double): List[MatchComputations] =
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



  def findMatches(products: Set[Product], listings: Set[Listing]): Map[Product, Iterable[MatchComputations]] =
    products.par.map({ p => p -> findConcreteMatches(p, listings) }).toList.toMap

  private[challenge] def findConcreteMatches(product: Product, listings: Iterable[Listing]):
  Iterable[MatchComputations] = {
    val potential = findPotentialMatches(product, listings)
    val filteredByPrice = AnalysisUtils.filterByPrice(potential.toList)
    filteredByPrice
  }

  /** Finds all listings that may match a particular product. Matches are made on one-to-one comparison basis. */
  private def findPotentialMatches(product: Product, listings: Iterable[Listing]): Set[MatchComputations] = {
    listings.foldLeft { Set[MatchComputations]() } { (seq: Set[MatchComputations], listing: Listing) =>
      val test = new MatchComputations(product, listing)
      if (test.isMatch) seq + test else seq
    }
  }
}
