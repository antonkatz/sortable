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
package com.sortable.challenge

/**
 * Utilities for breaking strings into tokens and processing of those tokens.
 */
object TokenizationUtils {
  /** A string and the position it was found at. */
  private[challenge] type Token = (String, Int)

  /** The only characters allowed to be in a token string. */
  private val cleanPattern = "[a-z0-9.]+".r

  /**
   * Tokens are bits of strings (either fully numeric or fully alphabetic) with a position attached. The position of
   * a token is where it was found in the passed in string.
   * @param delimiters a collection of characters that are used for the initial breaking down of the passed in string
   * @param str string from which the tokens are extracted.
   * @return a sequence of [[Token]]s that were obtained by breaking down the string.
   */
  private[challenge] def tokenizeWithIndex(str: String, delimiters: Set[Char], offset: Int = 0): Seq[Token] = {
    val tokens = findNext(str, delimiters) match {
      case None => splitTokenWithDigits(str, offset)
      case Some(tokenEnd) =>
        val token = str.substring(0, tokenEnd).trim
        val remaining = str.substring(tokenEnd + 1)
        val generated = splitTokenWithDigits(token, offset)
        generated ++ tokenizeWithIndex(remaining, delimiters, offset + tokenEnd + 1)
    }
    tokens filterNot { _._1 isEmpty }
  }

  /** Splits a token into fully numeric and fully alphabetic groups while preserving index. */
  private[challenge] def splitTokenWithDigits(token: Token): Seq[Token] = {
    token._1 exists { _ isDigit } match {
      case true =>
        val isFirstDigit = token._1.head isDigit
        val indexes = token._1.foldLeft { (isFirstDigit, 0, Seq[Int]()) } { (holder, char) =>
          val (isLastDigit, index, seq) = holder
          val isDigit = char.isDigit
          var updatedSeq = seq
          if (isDigit != isLastDigit) updatedSeq :+= index
          (isDigit, index + 1, updatedSeq)
        } _3
        val splitting = indexes.foldLeft { (0, Seq[Token]()) } { (holder, index) =>
          val (last, seq) = holder
          val t = (token._1 substring(last, index), last + token._2)
          (index, seq :+ t)
        }
        // adding the tail
        val t = (token._1 substring (splitting _1), splitting._1 + token._2)
        splitting._2 :+ t
      case false => Seq(token)
    }
  }

  /** Finds the closest next occurrence of any delimiter from the set. */
  private def findNext(str: String, delimiters: Set[Char]): Option[Int] = {
    val indices = delimiters map { c => str.indexOf(c) } filterNot { _ < 0 }
    if (indices.isEmpty) None else Option(indices.min)
  }

  /**
   * Removes non-alphanumeric characters (will drop parts of tokens after non-allowed characters), and shifts the
   * token to lowercase.
   * @return if there are several alphanumeric blocks in the token or if there are none returns [[None]], else the
   *         token with only alphanumeric characters
   */
  private[challenge] def strictCleanToken(token: Token): Option[Token] = {
    val (str, index) = token
    val matches = (cleanPattern findAllIn str.toLowerCase).toSeq
    if (matches.size == 1) Option((matches.head, index)) else None
  }
}
