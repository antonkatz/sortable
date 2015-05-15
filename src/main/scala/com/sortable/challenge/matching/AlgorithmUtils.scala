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

import com.sortable.challenge.matching.TokenMatchingUtils.TokenMatch

/**
 * Functions for testing and scoring matches between products and listings.
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
  def findTightestCluster(tokenMatches: Iterable[TokenMatch]): Iterable[TokenMatch] = {
    val tokenGroups = tokenMatches groupBy (_._2) mapValues (_ toList)
    val groupCount = tokenGroups.size
    val combinations = produceCombinationsFromBins(tokenGroups.values toList)
    val averagePositions = combinations map { _.map(_._3).sum.toDouble / groupCount }
    val distances = combinations zip averagePositions map { c =>
      val (comb, avg) = c
      comb -> { comb map { t => Math.abs(t._3 - avg) } sum }
    }
    distances minBy (_._2) _1
  }

  /** @return Average distance between token matches. */
  def computeAverageDispersion(tokens: Iterable[TokenMatch]): Double = {
    val boundaries = { tokens map { t => t._3 -> (t._3 + t._4.length) } toSeq } sortBy (_._1)
    if (boundaries.size < 2) return 0.0
    val gaps = boundaries.zip(boundaries.tail) map { b => b._2._1 - b._1._2 }
    gaps.sum.toDouble / gaps.size
  }

  /** Computes the percentage of occurrences of characters from one string (of) in a different string (to) without
    * replacement. */
  def simpleSimilarity(of: String, to: String): Option[Double] = {
    if (of.isEmpty || to.isEmpty) return None
    val toChars = to.toLowerCase.toBuffer
    val count: Int = of.toLowerCase.foldLeft (0) { (count: Int, char: Char) =>
      val index = toChars indexOf char
      if (index > -1) {
        toChars.remove(index)
        count + 1
      } else count
    }
    Option(count.toDouble / of.length)
  }

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

  private[matching] def produceCombinationsFromBins[T](choiceBins: List[List[T]]): List[List[T]] =
    choiceBins match {
      case head :: tail =>
        val tailComb = produceCombinationsFromBins(tail)
        tailComb map (t =>
          head map { h =>
            h :: t
          }) flatten
      case Nil => Nil :: Nil
    }
}
