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

import com.sortable.challenge.Listing
import com.sortable.challenge.TokenizationUtils.Token
import com.sortable.challenge.matching.TokenMatchType.TokenMatchType

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

  /**
   * Finds all positions of a sequence of tokens inside a destination string.
   * @param matchType example usage: indicating which attribute (name, manufacturer, etc.) a token came from and
   *                  which attribute (title, manufacturer, etc.) it was matched to
   * @return a sequence of [[TokenMatch]]es that have been found. A single [[Token]] can have any number
   *         [[TokenMatch]]es (including none)
   */
  def matchTokens(tokens: Iterable[Token], destination: String, matchType: TokenMatchType): Iterable[TokenMatch] =
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

  /** Removes [[TokenMatch]]es that have been found within other [[TokenMatch]]es. */
  def filterOvermatched(tokens: Iterable[TokenMatch]): Iterable[TokenMatch] = {
    val ranges = tokens map { t => t ->(t._3, t._3 + t._4.length) }
    ranges filterNot { tr =>
      ranges.exists(r => tr._2._1 >= r._2._1 && tr._2._2 <= r._2._2 && r._1._4.toLowerCase != tr._1._4.toLowerCase)
    } map { _._1 }
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
    // matches should never be empty
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

  def isNumeric(tokenMatch: TokenMatch): Boolean = tokenMatch._4 forall { _ isDigit }

  def isNumeric(token: Token): Boolean = token._1 forall { _ isDigit }

  /**
   * @usecase some model numbers are prefixed/suffixed with letters (also referred to as modifiers)
   * @return the closest fully alphabetic token(s) for each fully numeric token as found in the origin string
   */
  def getLettersAroundDigits(tokens: Set[Token]): Set[Token] = {
    val numeric = tokens filter isNumeric
    val alphabetic = tokens diff numeric
    numeric flatMap { n =>
      val modifiers = alphabetic map { a => a -> { if (n._2 > a._2) n._2 - a._2 else a._2 - n._2 - n._1.length } }
      // minBy is not safe
      if (modifiers isEmpty) return Set()
      val min = modifiers minBy (_._2) _2;
      modifiers filter { _._2 == min } map { _._1 }
    }
  }

  private def getLettersAroundDigits(matches: Iterable[TokenMatch]): Iterable[TokenMatch] = {
    val mT = matches map { m => matchToToken(m) -> m } toMap
    val aroundDigits = getLettersAroundDigits(mT.keySet)
    mT filterKeys (aroundDigits contains) values
  }

  private def matchToToken(m: TokenMatch): Token = (m._4, m._2)

  def getMissing(tokens: Set[Token], matches: Set[TokenMatch]): Set[Token] =
    tokens filterNot { t => matches exists (_._4 == t._1) }

  /** Determines if a match that if fully numeric has a digit following or preceding it in the destination string. */
  @deprecated
  def getImpureNumericMatches(matches: Iterable[TokenMatch], listing: Listing) = matches filter { isNumeric } filter {
    t =>
      val destinationString = TokenMatchType.getDestination(t._1, listing)
      (destinationString.lift(t._3 + t._4.length) exists { _ isDigit }) ||
          (destinationString.lift(t._3 - 1) exists { _ isDigit })
  }

  def getImpureMatches(matches: Iterable[TokenMatch], listing: Listing): Iterable[TokenMatch] = matches filter { t =>
    // no unicode
    def isImpure(c: Char) = if (isNumeric(t)) c isDigit else c isLetter
    val destinationString = TokenMatchType.getDestination(t._1, listing)
    (destinationString.lift(t._3 + t._4.length) exists isImpure) ||
        (destinationString.lift(t._3 - 1) exists isImpure)
  }

  /** @usecase Determines if a model modifier match has a digit between it and the closest fully numeric match. */
  def getImpurePhraseCount(matches: Iterable[TokenMatch], in: String): Int = {
    def range(m: TokenMatch) = m._3 -> (m._3 + m._4.length)

    val alphabeticRanges = getLettersAroundDigits(matches) map range
    val numericRanges = matches filter isNumeric map range
    val allRanges = {alphabeticRanges ++ numericRanges }.toSeq sortBy (_._1)
    if (allRanges.size < 2) return 0

    val impureZones = allRanges.zip(allRanges.tail) map { p => p._1._2 -> p._2._1 }
    impureZones count { p => in.substring(p._1, p._2).exists(_ isDigit) }
  }
}
