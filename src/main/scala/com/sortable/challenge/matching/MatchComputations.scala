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
  private var allTokenMatches = matchTokens(product.getNameTokens, listing.title, nameToTitle) ++
      matchTokens(product.getManufacturerTokens, listing.title, manufacturerToTitle) ++
      matchTokens(product.getModelTokens, listing.title, modelToTitle) ++
      matchTokens(product.getManufacturerTokens, listing.manufacturer, manufacturerToManufacturer)
  // family might not be present
  product.getFamilyTokens map { tokens =>
    matchTokens(tokens, listing.title, familyToTitle)
  } foreach { allTokenMatches ++= _ }

  /** All matched tokens grouped by their type (origin/destination). */
  private var groupedMatches = allTokenMatches groupBy (_._1)
  /** GroupedMatches with matches removed where the token is contained within a different matched token. */
  groupedMatches = groupedMatches mapValues TokenMatchingUtils.filterOvermatched

  /** The same token in the origin string can be matched several times in the destination string. Clustering attempts
    *  to address the issue of which one of those several matched positions should be considered. */
  private[matching] val clusters = groupedMatches mapValues TokenMatchingUtils.findTightestCluster

  /** The reordering of matched token in the destination string as compared to origin string. */
  private val orderChanges = clusters mapValues TokenMatchingUtils.getOrderChangeAroundPivot
  /** Order changes carry a penalty. This map is the sum of penalties for a particular type of tokens. */
  private val orderChangePenalties = orderChanges mapValues { _ map (o => Math.pow(o._2, 2)) sum }
  private[matching] val totalOrderChangePenalty = orderChangePenalties.values.toSeq sum
  private[matching] val modelOrderChangePenalty = orderChangePenalties getOrElse(modelToTitle, 0.0)

  /** Number of matched tokens, without considering how many matches a single token has. */
  private val distinctTokensCount = {clusters mapValues (_.size) values } sum
  /** Same as distinctTokensCount, but for numeric tokens. */
  private val distinctNumberTokensCount = {groupedMatches mapValues (_.count { _._4 forall (_ isDigit) }) values} sum
  /** How many tokens were not matched at least once. */
  private[matching] val missingCount = product.getTotalTokenCount - distinctTokensCount
  /** How many tokens (that are purely numeric) were not matched at least once. */
  private[matching] val missingNumberCount = product.getNumberTokenCount - distinctNumberTokensCount

  private val modelTokens = allTokenMatches filter (_._1 == modelToTitle)
  private val modelNumberTokens = modelTokens filter (_._4 forall { _ isDigit })
  private val modelNonNumberTokens = modelTokens diff modelNumberTokens
  // fixme rename
  private val modelModifierTokens = modelNumberTokens flatMap { n =>
    val modifiers = (modelNonNumberTokens map { nn => nn -> Math.abs(n._2 - nn._2) }) sortBy { _._2 }
    modifiers.headOption map { _._1 }
  }
  // tokens that have a digit following them
  // fixme. get string some other way. should not be in token.
  private val impureModelNumberTokensCount = modelNumberTokens count { t =>
    val originString = TokenMatchType.getOrigin(t._1, product)
    originString.lift(t._3 + t._4.length) exists { _ isDigit }
  }
  private val missingModelTokens = product.getModelTokens.size - modelTokens.groupBy(_._2).size
  private val missingModelModifiers = modelModifierTokens groupBy (_._2) map (_._2.head) count {
    !modelTokens
        .contains(_)
  }

  //	private val dispersions = clusters mapValues {MatchingUtils.computeRelativeVariance(_, 1024)}
  private val dispersions = clusters mapValues { TokenMatchingUtils.computeRelativePositionalVariance }
  private val nonZeroDispersions = dispersions filterNot (d => d._2 < 0.001 && d._2 > -0.001)
  private val avgDispersion = (nonZeroDispersions.values sum) / nonZeroDispersions.size

  // fixme move out of here to anaylsisutils
  /**
   * Whether this tests was successful (the product and the listing are a match).
   */
  def isMatch: Boolean = totalOrderChangePenalty < 5 &&
      modelOrderChangePenalty <= 2 &&
      (missingCount.toDouble + impureModelNumberTokensCount) / product.getTotalTokenCount < 0.5 &&
      (missingModelTokens.toDouble + missingModelModifiers) / product.getModelTokens.size <= 0.5 &&
      missingNumberCount.toDouble / product.getNumberTokenCount <= 0.5 &&
      avgDispersion < 20 + { nonZeroDispersions.get(TokenMatchType.modelToTitle) map (d => 15 / (d + 1)) getOrElse 0.0 }
}
