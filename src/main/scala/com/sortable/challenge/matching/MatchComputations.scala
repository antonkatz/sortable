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

import com.sortable.challenge.matching.TokenMatchType._
import com.sortable.challenge.matching.TokenMatchingUtils._
import com.sortable.challenge.{Listing, Product}

import scala.language.postfixOps

/**
 * Does a multitude of computations used to determine whether a product and a listing are a match.
 */
case class MatchComputations(product: Product, listing: Listing) {
  private val tokensByType = TokenMatchType.values map {tt =>
    tt -> TokenMatchType.getConstituents(tt, product, listing)} filterNot {_._2._1 isEmpty} toMap

  /** The maximum possible number of [[com.sortable.challenge.matching.TokenMatchingUtils.TokenMatch]]es. */
  private[challenge] val totalTokenCount = tokensByType map {_._2._1 size} sum
  /** Same as totalTokenCount, but for fully numeric tokens only. */
  private[challenge] val totalNumberTokenCount = tokensByType map {_._2._1 count isNumeric} sum

  /** All matched tokens grouped by their type (origin/destination). */
  private var groupedMatches = tokensByType map {t =>
    t._1 -> matchTokens(t._2._1, t._2._2, t._1)} filterNot {_._2 isEmpty }
  /** GroupedMatches with matches removed where the token is contained within a different matched token. */
  groupedMatches = groupedMatches mapValues TokenMatchingUtils.filterOvermatched

  /** The same token in the origin string can be matched several times in the destination string. Clustering attempts
    * to address the issue of which one of those several matched positions should be considered. */
  private[matching] val clusters = groupedMatches mapValues TokenMatchingUtils.findTightestCluster

  /** The reordering of matched token in the destination string as compared to origin string. */
  private val orderChanges = clusters mapValues TokenMatchingUtils.getOrderChangeAroundPivot
  /** Order changes carry a penalty. This map is the sum of penalties for a particular type of tokens. */
  private val orderChangePenalties = orderChanges mapValues { _ map (o => Math.pow(o._2, 2)) sum }
  private[matching] val totalOrderChangePenalty = orderChangePenalties.values.toSeq sum
  private[matching] val modelOrderChangePenalty = orderChangePenalties getOrElse(modelToTitle, 0.0)

  /** Number of matched tokens, without considering how many matches a single token has. */
  private val distinctMatchCount = {clusters mapValues (_.size) values } sum
  /** Same as distinctTokensCount, but for numeric tokens. */
  private val distinctNumberMatchCount = {groupedMatches mapValues (_ count isNumeric) values } sum
  /** How many tokens were not matched at least once. */
  private[matching] val missingCount = totalTokenCount - distinctMatchCount
  /** How many tokens (that are purely numeric) were not matched at least once. */
  private[matching] val missingNumberCount = totalNumberTokenCount - distinctNumberMatchCount

  private val modelTokensMatches = clusters getOrElse(modelToTitle, Iterable()) toSeq
  /** Model numbers often have a letter or a combination of letters acting as a 'modifier'. */
  private val modelModifierTokens = getLettersAroundDigits(product.getModelTokens toSet)
  /** Numeric tokens present in the model of the product that have a digit following in them in the destination
    * string. */
  private[matching] val missingModelTokensCount = product.getModelTokens.size - modelTokensMatches.size
  private[matching] val missingModelModifiers = getMissing(modelModifierTokens toSet, modelTokensMatches toSet)
  private[matching] val impureModelNumberTokensCount = getImpureNumericMatches(modelTokensMatches, listing) size
  private[matching] val impureModelModifiersCount = getImpurePhraseCount(modelTokensMatches, listing.title)

  /** Matched tokens from an origin string will be separated by some distance, which serves as an indication of how
    * relevant the token matches are for determining if the listing is a match to a product. */
  private val dispersions = clusters mapValues { TokenMatchingUtils.computeAverageDispersion }
  private val nonZeroDispersions = dispersions filterNot (d => d._2 < 0.001 && d._2 > -0.001)
  /** The average dispersion across all token types. */
  private[matching] val avgDispersion = (nonZeroDispersions.values sum) / nonZeroDispersions.size

  // fixme move out of here to anaylsisutils
  /** Model tokens play an important role in determining if a listing is a match to a product. Tightly clustered
    * model tokens can offset the global (average) dispersion limit. */
  private[matching] val dispersionLimitOffset =
    nonZeroDispersions.get(TokenMatchType.modelToTitle) map (d => 4 / (d + 1)) getOrElse 0.0

  val missingModelModifierPenalty = missingModelModifiers map {t => 1.2 - (t._1.length / 4.0)} filter {_ > 0} sum

  // fixme move out of here to anaylsisutils
  /**
   * Whether this tests was successful (the product and the listing are a match).
   */
  def isMatch: Boolean = totalOrderChangePenalty < 5 &&
      modelOrderChangePenalty <= 2 &&
      (missingCount.toDouble + impureModelNumberTokensCount) / totalTokenCount < 0.5 &&
      (missingModelTokensCount.toDouble + missingModelModifierPenalty) / product.getModelTokens.size <= 0.5 &&
      missingNumberCount.toDouble / totalNumberTokenCount <= 0.5 &&
      avgDispersion < 5 + dispersionLimitOffset &&
      (impureModelModifiersCount < modelModifierTokens.size || modelModifierTokens.isEmpty)
}
