/*
The MIT License (MIT)

Copyright (c) 2015 Anton Kats

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
package com.sortable.challenge.matching

import com.sortable.challenge.{Listing, Product}

/**
 * Does 'scoring'/filtering and determines product to listing matches
 */
object Algorithm {
  private val debugOn = true

  /** A one-on-one product to listing comparison must satisfy all these conditions to move on the next round of 
    * filtering (which is filtering by price at the moment). */
  private val matchingConditions = Seq[(PairHolder) => Boolean](
    orderChange, modelOrderChange,
    missing, missingModel, missingNumber, //4
    dispersion, modelDispersion,
    modelModifiers, manufacturer, clusterDifference
  )

  private val maxPriceSDGap = 0.8

  private val impurityFilterType = TokenMatchType.modelToTitle

  private val impurityFilterMultiplier = 2

  /**
   * The only entry point into the algorithm.
   * @return a map with each product as key having a collection of [[PairHolder]]s that have been deemed as a match
   */
  def findMatches(products: Iterable[Product], listings: Iterable[Listing]): Map[Product, Iterable[PairHolder]] =
    products.par.map({ p => p -> findMatches(p, listings) }).toList.toMap

  /*
  At this stage of development it makes more sense to keep limits and other configurable properties within the simple
  functions that make up the bulk of decision making.
   */
  private def ocp(o: Int) = Math.pow(o, 2)

  private def orderChangePenalty(changes: Iterable[Int]) = changes map ocp sum

  private def modelOrderChangePenalty(p: PairHolder) =
    p.Model.orderChange map { c => orderChangePenalty(c.values) } getOrElse 0.0

  /* Conditions */

  /** In an ideal match tokens should not swap places or change order. */
  private def orderChange(p: PairHolder) = {
    val count = p.Global.orderChanges map {_._2 size} sum
    val penalty = p.Global.orderChanges map { c => orderChangePenalty(c._2.values) } sum;
    penalty < count // fixme 5
  }

  private def modelOrderChange(p: PairHolder) = modelOrderChangePenalty(p) <= 2

  /** The less tokens that have not been matched, the better. */
  private def missing(p: PairHolder) = {
    if (p.Global.totalTokenCount == 0) true
    else (p.Global.missingCount + p.Global.impureMatchesCount).toDouble / p.Global.totalTokenCount < 0.5
  }

  private def missingModel(p: PairHolder) = {
    if (p.Model.tokenCount == 0) true
    else {
      // short alphabetic model tokens, when missing carry a penalty proportionate to their size
      val missingModifierPenalty =
        p.Model.missingMatches map { t => 1.2 - (t._1.length / 4.0) } filter { _ > 0 } sum;
      (p.Model.missingModelTokensCount.toDouble + missingModifierPenalty) / p.Model.tokenCount <= 0.5
    }
  }

  /** Purely numeric tokens that have not been matched. */
  private def missingNumber(p: PairHolder) = {
    p.Global.totalNumberTokenCount == 0 || p.Global.missingNumberCount.toDouble / p.Global.totalNumberTokenCount <= 0.5
  }

  private def dispersion(p: PairHolder) = {
    /* Model tokens play an important role in determining if a listing is a match to a product. Tightly clustered
     model tokens can offset the global (average) dispersion limit */
    val dispersionOffset =
      p.Global.nonZeroDispersions.get(TokenMatchType.modelToTitle) map (d => 4 / (d + 1)) getOrElse 0.0
    /* If there are no missing tokens, a pair will get a huge offset */
    val presentOffset = 6 / Math.pow(p.Global.missingCount + 1, 2)
    p.Global.avgDispersion < 6 + dispersionOffset + presentOffset
  }

  private def modelDispersion(p: PairHolder) = {
    val nonUnique = p.Model.allMatches diff (p.Model.uniqueMatches toSeq)
    val dispersion = AlgorithmUtils.computeAverageDispersion(nonUnique)
    if (debugOn) p.debug += ("mnud" -> dispersion)
    /* Generally small tokens (especially letters) are matched often which is fine. But sometimes they are part of a
    series of related models (but not the model of the product in question), which this penalty attempts to detect. */
    val extraModelDispersionPenalty =
      if (dispersion > 4 && dispersion <= 11 && p.Model.allMatchesCount / 2.5 > p.Model.tokenCount) dispersion else 0.0
    p.Model.dispersion + extraModelDispersionPenalty < 3
  }

  private def modelModifiers(p: PairHolder): Boolean = {
    if (p.Model.modifierTokens.isEmpty) return true
    // recalculating order change penalty, rather than storing it in PairHolder
    val orderChangePenalty = modelOrderChangePenalty(p)
    // in cases where model tokens have changed order, impurity of those tokens is less important
    val impurityScore =
      if (orderChangePenalty >= 1.0) p.Model.impurePhraseCount / orderChangePenalty
      else p.Model.impurePhraseCount
    impurityScore < p.Model.modifiersCount
  }

  private def manufacturer(p: PairHolder) = {
    p.Manufacturer.manufacturerMatchCount.toDouble / p.Manufacturer.tokenCount > 0.5 ||
        p.Manufacturer.manufacturerSimilarity.map(_ > 0.5).getOrElse(true)
  }

  /** In an ideal product/listing match, the token matches from product name should be at the same positions as the
    *  model matches. */
  private def clusterDifference(p: PairHolder) = {
    p.Model.clusterDifferenceCount.toDouble / p.Model.tokenCount <= 0.5
  }

  /* --- */

  private[matching] def findMatches(product: Product, listings: Iterable[Listing]):
  Iterable[PairHolder] = {
    val potential = findPotentialMatches(product, listings)
    val filteredByPrice = filterByPrice(potential.toList)
    val filteredByImpurity = filterByImpurity(filteredByPrice)
    filteredByImpurity
  }

  /** Finds all listings that may match a particular product. Matches are made on one-to-one comparison basis. */
  private def findPotentialMatches(product: Product, listings: Iterable[Listing]): Iterable[PairHolder] = {
    /* Not a set because some listings are identical */
    listings.foldLeft { Seq[PairHolder]() } { (seq: Seq[PairHolder], listing: Listing) =>
      val pair = new PairHolder(product, listing)
      if (isPotentialMatch(pair)) seq :+ pair else seq
    }
  }

  // fixme remove
  private[matching] def isPotentialMatch(pair: PairHolder): Boolean = {
    val statuses = matchingConditions map { _(pair) }
    val pass = statuses forall { _ == true }
    if (debugOn) pair.debugConditions = statuses
    pass
  }

  private def filterByPrice(pairs: List[PairHolder]): List[PairHolder] =
    AlgorithmUtils.filterByPriceGap(pairs, maxPriceSDGap)

  private def filterByImpurity(pairs: Iterable[PairHolder]): Iterable[PairHolder] = {
    AlgorithmUtils.getPerfectImpurityAvg(pairs, impurityFilterType) map { perfectLevel =>
      val limit = perfectLevel * impurityFilterMultiplier
      pairs filter { p =>
        p.Global.clusters get impurityFilterType map { matchesOfType =>
          val impure = TokenMatchingUtils.getStrictImpureMatches(matchesOfType, p.listing).size
          if (debugOn) p.debug += ("fii" -> impure, "fil" -> limit)
          impure <= limit
        } getOrElse true;
      }
    } getOrElse pairs
  }
}
