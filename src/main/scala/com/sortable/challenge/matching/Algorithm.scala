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
    missing, missingModel, missingNumber,
    dispersion, modelDispersion,
    modelModifiers, manufacturer
  )

  private val maxPriceSDGap = 0.9

  /**
   * The only entry point into the algorithm.
   * @return a map with each product as key having a collection of [[PairHolder]]s that have been deemed as a match
   */
  def findMatches(products: Set[Product], listings: Set[Listing]): Map[Product, Iterable[PairHolder]] =
    products.par.map({ p => p -> findMatches(p, listings) }).toList.toMap

  /*
  At this stage of development it makes more sense to keep limits and other configurable properties within the simple
  functions that make up the bulk of decision making.
   */
  private def ocp(o: Int) = Math.pow(o, 2)

  private def orderChangePenalty(changes: Iterable[Int]) = changes map ocp sum

  private def modelOrderChangePenalty(p: PairHolder) =
    p.Model.orderChange map { c => orderChangePenalty(c.values) } getOrElse 0.0

  /** In an ideal match tokens should not swap places or change order. */
  private def orderChange(p: PairHolder) = {
    val penalty = p.Global.orderChanges map { c => orderChangePenalty(c._2.values) } sum;
    penalty < 5
  }

  private def modelOrderChange(p: PairHolder) = modelOrderChangePenalty(p) <= 2

  /** The less tokens that have not been matched, the better. */
  private def missing(p: PairHolder) = {
    if (p.Global.totalTokenCount == 0) true
    else (p.Global.missingCount + p.Global.impureTokensCount).toDouble / p.Global.totalTokenCount < 0.5
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
     model tokens can offset the global (average) dispersion limit. */
    val dispersionOffset =
      p.Global.nonZeroDispersions.get(TokenMatchType.modelToTitle) map (d => 4 / (d + 1)) getOrElse 0.0
    p.Global.avgDispersion < 6 + dispersionOffset
  }

  private def modelDispersion(p: PairHolder) = {
    val nonUnique = p.Model.allMatches diff (p.Model.uniqueMatches toSeq)
    val dispersion = TokenMatchingUtils.computeAverageDispersion(nonUnique)
    /* Generally small tokens (especially letters) are matched often which is fine. But sometimes they are part of a
    series of related models (but not the model of the product in question), which this penalty attempts to detect. */
    val extraModelDispersionPenalty =
      if (dispersion < 12 && p.Model.allMatchesCount / 2.5 > p.Model.tokenCount) dispersion else 0.0
    p.Model.dispersion + extraModelDispersionPenalty < 3
  }

  private def modelModifiers(p: PairHolder): Boolean = {
    if (p.Model.modifierTokens.isEmpty) return true
    // recalculating order change penalty, rather than storing it in PairHolder
    val orderChangePenalty = modelOrderChangePenalty(p)
    // in cases where model tokens have changed order, impurity of those tokens is less important
    val impurityScore =
      if (orderChangePenalty >= 1.0) p.Model.impureModelPhraseCount / orderChangePenalty
      else p.Model.impureModelPhraseCount
    impurityScore < p.Model.modifiersCount
  }

  private def manufacturer(p: PairHolder) = {
    p.Manufacturer.manufacturerMatchCount.toDouble / p.Manufacturer.tokenCount > 0.5 ||
        p.Manufacturer.manufacturerSimilarity.map(_ > 0.5).getOrElse(true)
  }

  /* --- */

  private[matching] def findMatches(product: Product, listings: Iterable[Listing]):
  Iterable[PairHolder] = {
    val potential = findPotentialMatches(product, listings)
    val filteredByPrice = Algorithm.filterByPrice(potential.toList)
    filteredByPrice
  }

  /** Finds all listings that may match a particular product. Matches are made on one-to-one comparison basis. */
  private def findPotentialMatches(product: Product, listings: Iterable[Listing]): Set[PairHolder] = {
    listings.foldLeft { Set[PairHolder]() } { (seq: Set[PairHolder], listing: Listing) =>
      val pair = new PairHolder(product, listing)
      if (isPotentialMatch(pair)) seq + pair else seq
    }
  }

  private def isPotentialMatch(pair: PairHolder): Boolean = {
    val statuses = matchingConditions map { _(pair) }
    val pass = statuses forall { _ == true }
    if (debugOn) pair.debug = statuses
    pass
  }

  private def filterByPrice(matches: List[PairHolder]): List[PairHolder] =
    AlgorithmUtils.filterByPriceGap(matches, maxPriceSDGap)
}
