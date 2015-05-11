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

import com.sortable.challenge.TokenizationUtils.Token
import com.sortable.challenge.matching.TokenMatchType.TokenMatchType
import com.sortable.challenge.Product

import scala.language.postfixOps

/**
 * Methods for finding positions of tokens across attributes
 */
object TokenMatchingUtils {
	/**
	 * Type, position in the original attribute, position in the destination (searched in) attribute/string.
	 * The positions are the start positions of a token string inside a string, not relative positions to each other.
	 * The second element of the triple can serve as id.
	 *
	 * FIXME
	 * 4th element is the token string
	 * 5th is the destination string
	 */
	type TokenMatch = (TokenMatchType, Int, Int, String)

	/** Removes [[TokenMatch]]es that have been found within other [[TokenMatch]]es. */
	def filterOvermatched(tokens: Seq[TokenMatch]): Seq[TokenMatch] = {
		val ranges = tokens map { t => t ->(t._3, t._3 + t._4.length) }
		ranges filterNot { tr =>
			ranges.exists(r => tr._2._1 >= r._2._1 && tr._2._2 <= r._2._2 && r._1 != tr._1)
		} map { _._1 }
	}

	/** @return Average variance proportional to the average length of the tokens. */
	def computeRelativePositionalVariance(tokens: Iterable[TokenMatch]) = {
		val size = tokens.size toDouble
		val pos = tokens map { _._3 }
		val avg = pos.sum / size
		val avgLength = (tokens map { _._4 length } sum) / size
		((pos map { p => Math.pow(p - avg, 2) } sum) / size) / avgLength
	}

	/**
	 * Given a sequence of [[TokenMatch]] (eg. an original phrase), calculates the order change of tokens in the
	 * destination sequence compared to the original sequence (around a pivot - first token in original sequence).
	 * @param matches a sequence of [[TokenMatch]]es of tokens that appear in the original string. A single
	 *                token can have several [[TokenMatch]]es
	 * @return a map with [[TokenMatch]] as keys, and displacement as values. To be most fair, given that a token
	 *         can have several [[TokenMatch]]es, only the best (shortest) displacement is returned
	 */
	def getOrderChangeAroundPivot(matches: Iterable[TokenMatch]): Map[TokenMatch, Int] = {
		val wordIndexed = toWordIndex(matches toSeq)
		val destinationPivotPosition = {wordIndexed find { t => t._2._1 == 0 } get }._2._2
		wordIndexed map { t => t._1 -> (t._2._2 - t._2._1 - destinationPivotPosition) }
	}

	/** Produces a map where [[TokenMatch]]es are keys, and values are tuple of indices relative to the first token in a
	  * sequence/string. The values are (original index, destination index). */
	private def toWordIndex(matches: Seq[TokenMatch]): Map[TokenMatch, (Int, Int)] = {
		val pivot = matches minBy (_._2)
		// the position in destination of the first token in the original
		val originalPivotPosition = pivot._2
		val destinationPivotPosition = pivot._3

		val originalIndices = {
			matches map (t => t -> (t._2 - originalPivotPosition)) sortBy (_._2) zipWithIndex
		} map (t => t._1._1 -> t._2)
		val destinationIndices = {
			matches map (t => t -> (t._3 - destinationPivotPosition)) sortBy (_._2) zipWithIndex
		} map (t => t._1._1 -> t._2) toMap;
		originalIndices map { t => t._1 ->(t._2, destinationIndices(t._1)) } toMap
	}

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

	/**
	 * Finds all positions of a sequence of tokens inside a destination string.
	 * @param matchType example usage: indicating which attribute (name, manufacturer, etc.) a token came from and
	 *                  which attribute (title, manufacturer, etc.) it was matched to
	 * @return a sequence of [[TokenMatch]]es that have been found. A single [[Token]] can have any number
	 *         [[TokenMatch]]es (including none)
	 */
	def matchTokens(tokens: Seq[Token], destination: String, matchType: TokenMatchType): Seq[TokenMatch] =
		tokens map { t =>
			val (tokenStr, index) = t
			findAllWithIndex(tokenStr, destination) map { destIndex => (matchType, index, destIndex, tokenStr) }
		} flatten

	/** Finds all positions of a string in another string. */
	private def findAllWithIndex(what: String, in: String): Seq[Int] = {
		// non-recursive form used for performance (couldn't call-tail optimize)
		var offset = 0
		var lastIndex = in indexOf what
		var indices = Seq[Int]()
		while (lastIndex != -1) {
			indices :+= lastIndex
			offset = lastIndex + 1
			lastIndex = in indexOf(what, offset)
		}
		indices
	}
}
