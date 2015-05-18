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

import com.sortable.challenge.matching.TokenMatchType.TokenMatchType
import com.sortable.challenge.matching.TokenMatchingUtils.TokenMatch

/**
 * Functions to be used in testing and scoring matches between products and listings.
 * These are somewhat algorithm dependant.
 */
object AlgorithmUtils {
  /**
   * Finds a sequence of [[TokenMatch]]es that are most closely grouped in the destination string. There is a
   * possibility of ties occurring, but the chances are deemed insignificant.
   * @param tokenMatches a collection of [[TokenMatch]] where tokens (from which the [[TokenMatch]]es were produced)
   *                     can repeat
   * @return [[TokenMatch]]es with no tokens repeating
   */
  private[matching] def findTightestCluster(tokenMatches: Iterable[TokenMatch]): Iterable[TokenMatch] = {
    /* todo this should not try to find the tightest cluster, but the one most similar to the one in the listing.
     * "model-num" should match "model num" more readily than "modelnum" */
    val tokenGroups = tokenMatches groupBy (_._2) mapValues (_ toList)
    val groupCount = tokenGroups.size
    val combinations = producePermutationsFromBins(tokenGroups.values toList)
    val averagePositions = combinations map { _.map(_._3).sum.toDouble / groupCount }
    val distances = combinations zip averagePositions map { c =>
      val (comb, avg) = c
      comb -> { comb map { t => Math.abs(t._3 - avg) } sum }
    }
    distances minBy (_._2) _1
  }

  /** @return Average distance between token matches. */
  private[matching] def computeAverageDispersion(tokens: Iterable[TokenMatch]): Double = {
    val boundaries = { tokens map { t => t._3 -> (t._3 + t._4.length) } toSeq } sortBy (_._1)
    if (boundaries.size < 2) return 0.0
    val gaps = boundaries.zip(boundaries.tail) map { b => b._2._1 - b._1._2 }
    gaps.sum.toDouble / gaps.size
  }

  /** @return the percentage of occurrences of characters from one string in a different string without
    *         replacement. */
  private[matching] def simpleStringSimilarity(of: String, to: String): Option[Double] = {
    if (of.isEmpty || to.isEmpty) return None
    val toChars = to.toLowerCase.toBuffer
    val count: Int = of.toLowerCase.foldLeft(0) { (count: Int, char: Char) =>
      val index = toChars indexOf char
      if (index > -1) {
        toChars.remove(index)
        count + 1
      } else count
    }
    Option(count.toDouble / of.length)
  }

  /**
   * Given a collection of matches that contain both fully numeric tokens and fully alphabetic tokens, determines how
   * many instances there are where a digit comes between an alphabetic match and a numeric match. 
   * @usecase Determines if a model modifier match has a digit between it and the closest fully numeric match, which
   *          implies a worse quality match between a product and a listing.
   */
  private[matching] def getImpureNumericPhraseCount(matches: Iterable[TokenMatch], in: String): Int = {
    def range(m: TokenMatch) = m._3 -> (m._3 + m._4.length)

    val alphabeticRanges = TokenMatchingUtils.getLettersAroundDigits(matches) map range
    val numericRanges = matches filter TokenMatchingUtils.isNumeric map range
    val allRanges = {alphabeticRanges ++ numericRanges }.toSeq.distinct sortBy (_._1)
    if (allRanges.size < 2) return 0

    val impureZones = allRanges.zip(allRanges.tail) map { p => p._1._2 -> p._2._1 }
    impureZones count { p => in.substring(p._1, p._2).exists(_ isDigit) }
  }

  /**
   * @usecase Once [[PairHolder]]es pass one-to-one stage of the algorithm, some of the matches in a collection of
   *          matches for a particular product may contain items that differ significantly from the majority based on
   *          price (eg. accessories). In those cases the prices tend to fall into groups, rather than occupying a
   *          continuous space.
   *          This method finds gaps in the prices, and based on those gaps splits the prices into groups. The
   *          largest group is the one that passes this filter.
   * @param pairs all potential matches for a single product
   * @param maxSdGap maximum number of standard deviations between two price points for the space between those
   *                 points to be considered a gap
   * @return a list of [[PairHolder]]s that are not obviously overpriced or underpriced.
   */
  private[matching] def filterByPriceGap(pairs: List[PairHolder], maxSdGap: Double): List[PairHolder] = {
    val sd = computePriceSd(pairs)
    val sortedByPrice =
      pairs map { m => m -> m.listing.adjustedPrice } collect { case (m, Some(p)) => m -> p } sortBy (_._2)
    val sortedPrices = sortedByPrice map (_._2)
    val sortedMatches = sortedByPrice map (_._1)

    sortedPrices match {
      case head :: tail =>
        val neighbourDistances = sortedPrices.tail.foldLeft { (sortedPrices.head, List[Double]()) } {
          (f: (Double, List[Double]), p: Double) => (p, f._2 :+ { p - f._1 })
        } _2
        var gapPositions: List[Int] =
          neighbourDistances.zipWithIndex collect { case (d, i) if d / sd > maxSdGap => i + 1 } sorted;
        gapPositions = 0 +: gapPositions :+ sortedByPrice.length
        val clusters = { gapPositions zip gapPositions.tail } map { case (s, e) => sortedMatches.slice(s, e) }
        clusters.sortBy(_.size) last
      case _ => pairs
    }
  }

  /**
   * Filters for [[PairHolder]]es that have no missing tokens, and calculates their average strict impurity.
   * @usecase When comparing [[PairHolder]] for a product between each other, a useful metric is to compare strict
   *          impurities count to that of the best matches
   * @param pairs a collection of [[PairHolder]]es for a particular product
   * @param ofType [[TokenMatchType]] of tokens to consider when computing the impurities
   * @return an average impurity of the best tokens, if there are any best tokens
   * @see [[TokenMatchingUtils.getStrictImpureMatches]]
   */
  private[matching] def getPerfectImpurityAvg(pairs: Iterable[PairHolder], ofType: TokenMatchType): Option[Double] = {
    val perfect = pairs filter { _.Global.missingCount == 0 }
    val impurities = perfect map { pair =>
      pair.Global.clusters get ofType map { matchesOfType =>
        TokenMatchingUtils.getStrictImpureMatches(matchesOfType, pair.listing) size
      }
    } flatten;
    if (impurities isEmpty) {
      None
    } else {
      Option(impurities.sum.toDouble / impurities.size)
    }
  }

  /** @return standard deviation of all values in a collection */
  private def computeSD(iterable: Iterable[Double]): Double = {
    val size = iterable.size
    val avg = iterable.sum / size
    val variance = (iterable map { i => Math.pow(i - avg, 2) } sum) / size
    Math.sqrt(variance)
  }

  /** @return standard deviation of prices of a collection of listings in
    *         [[com.sortable.challenge.matching.PairHolder]]es */
  private def computePriceSd(matches: Iterable[PairHolder]): Double = computeSD(getPrices(matches))

  /** @return the adjusted price (by currency) for a listing in a [[PairHolder]]*/
  private def getPrices(matches: Iterable[PairHolder]) = matches map { _.listing.adjustedPrice } flatten

  /**
   * For example, given 3 bins containing [0, 1], [2], [3, 4] this method will produce a list containing all of
   * [0, 2, 3] [0, 2, 4] [1, 2, 3] [1, 2, 4]
   * @param choiceBins a list of bins, where each bin is a list of possible choices
   * @return all permutations of possible choices, where a position in the permutation can only be filled from a
   *         respective bin
   **/
  private[matching] def producePermutationsFromBins[T](choiceBins: List[List[T]]): List[List[T]] =
    choiceBins match {
      case head :: tail =>
        val tailComb = producePermutationsFromBins(tail)
        tailComb map (t =>
          head map { h =>
            h :: t
          }) flatten
      case Nil => Nil :: Nil
    }
}
