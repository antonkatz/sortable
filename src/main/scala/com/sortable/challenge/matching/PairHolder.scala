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
import com.sortable.challenge.matching.AlgorithmUtils._
import com.sortable.challenge.{Listing, Product}

import scala.language.postfixOps

/**
 * Does a multitude of computations used to determine whether a product and a listing are a match.
 */
case class PairHolder(product: Product, listing: Listing) {

  object Global {
    private val tokensByType = TokenMatchType.values map { tt =>
      tt -> TokenMatchType.getConstituents(tt, product, listing)
    } filterNot { _._2._1 isEmpty } toMap

    /** The maximum possible number of [[com.sortable.challenge.matching.TokenMatchingUtils.TokenMatch]]es. */
    private[matching] val totalTokenCount = tokensByType map { _._2._1 size } sum
    /** Same as totalTokenCount, but for fully numeric tokens only. */
    private[matching] val totalNumberTokenCount = tokensByType map { _._2._1 count isNumeric } sum

    /** All matched tokens grouped by their type (origin/destination). */
    private[PairHolder] var groupedMatches = tokensByType map { t =>
      t._1 -> matchTokens(t._2._1, t._2._2, t._1)
    } filterNot { _._2 isEmpty }
    /** GroupedMatches with matches removed where the token is contained within a different matched token. */
    groupedMatches = filterOvermatched(groupedMatches.values flatten) groupBy (_._1)

    /** The same token in the origin string can be matched several times in the destination string. Clustering attempts
      * to address the issue of which one of those several matched positions should be considered. */
    private[PairHolder] val clusters = groupedMatches mapValues findTightestCluster

    /** The reordering of matched token in the destination string as compared to origin string. */
    private[matching] val orderChanges = clusters mapValues getOrderChangeAroundPivot

    /** Number of matched tokens, without considering how many matches a single token has within a type category.
      * If a token with the same string value is present across different [[TokenMatchType]] categories it will be 
      * counted twice. */
    private val typedDistinctMatchCount = {clusters mapValues (_.size) values } sum
    /** Same as distinctTokensCount, but for numeric tokens. */
    private val typedDistinctNumberMatchCount = {clusters mapValues (_ count isNumeric) values } sum
    /** How many tokens were not matched at least once. */
    private[matching] val missingCount = totalTokenCount - typedDistinctMatchCount
    /** How many tokens (that are purely numeric) were not matched at least once. */
    private[matching] val missingNumberCount = totalNumberTokenCount - typedDistinctNumberMatchCount
    private[matching] val impureTokensCount = {clusters mapValues { getImpureMatches(_, listing).size } values } sum

    /** Matched tokens from an origin string will be separated by some distance, which serves as an indication of how
      * relevant the token matches are for determining if the listing is a match to a product. */
    private val dispersions = clusters mapValues { computeAverageDispersion }
    private[matching] val nonZeroDispersions = dispersions filterNot (d => d._2 < 0.001 && d._2 > -0.001)
    /** The average dispersion across all token types. */
    private[matching] val avgDispersion = (nonZeroDispersions.values sum) / nonZeroDispersions.size
  }

  object Model {
    private[matching] val allMatches = Global.groupedMatches.getOrElse(modelToTitle, Iterable()) toSeq
    private[matching] val uniqueMatches = Global.clusters getOrElse(modelToTitle, Iterable()) toSeq

    /** Model numbers often have a letter or a combination of letters acting as a 'modifier'. */
    private[matching] val modifierTokens = getLettersAroundDigits(product.getModelTokens toSet)
    private[matching] val modifiersCount = modifierTokens.size
    /** Numeric tokens present in the model of the product that have a digit following in them in the destination
      * string. */
    private[matching] val missingModelModifiers = getMissing(modifierTokens.toSet, uniqueMatches.toSet)
    private[matching] val missingMatches = getMissing(product.getModelTokens.toSet, uniqueMatches.toSet)

    /** The number of tokens extracted from the [[Product]]. */
    private[matching] val tokenCount = product.getModelTokens.length
    private[matching] val allMatchesCount = allMatches.length
    private[matching] val missingModelTokensCount = tokenCount - uniqueMatches.size
    private[matching] val impureModelPhraseCount = getImpurePhraseCount(uniqueMatches, listing.title)

    private[matching] val orderChange = Global.orderChanges get modelToTitle

    private[matching] val dispersion = Global.nonZeroDispersions.getOrElse(modelToTitle, 0.0)
  }

  object Manufacturer {
    val tokenCount = product.getManufacturerTokens.size
    val manufacturerMatchCount = Global.clusters.getOrElse(manufacturerToTitle, Set()).size
    val manufacturerSimilarity = simpleSimilarity(product.manufacturer, listing.manufacturer)
  }

  /** Used to debug purposes. When the [[com.sortable.challenge.matching.Algorithm]] makes its decision, it will dump
    *  pass/fail status of a sequence of conditions into this variable. */
  private[matching] var debug = Seq[Boolean]()
}
