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
 *
 */
object TokenizationUtils {
	type Token = (String, Int)

	private val cleanPattern = "[a-z0-9.]+".r

	private val numberPattern = "[0-9]+".r

	// also splits tokens with digits into two tokens
	def tokenizeWithIndex(str: String, delimiters: Set[Char], offset: Int = 0): Seq[Token] = {
		val tokens = findNext(str, delimiters) match {
			case None => splitTokenWithNumber (str, offset)
			case Some(tokenEnd) =>
				val token = str.substring(0, tokenEnd).trim
				val remaining = str.substring(tokenEnd + 1)
				val generated = splitTokenWithNumber (token, offset)
				generated ++ tokenizeWithIndex(remaining, delimiters, offset + tokenEnd + 1)
		}
		tokens filterNot {_._1 isEmpty}
	}

	// always splits digits into two tokens, no matter how many groups exist
	private[challenge] def splitTokenWithNumber(token: Token): Seq[Token] = {
		token._1 find {_ isDigit} match {
			case Some(d) =>
				val firstNumber = if (token._1.head isDigit) numberPattern findFirstIn token._1 get else ""
				val firstNumberOffset = firstNumber.length
				val i = token._1 indexOf d
				val offset = token._2
				Seq((token._1 substring(0, i + firstNumberOffset), offset),
					(token._1 substring(i + firstNumberOffset), i + offset + firstNumberOffset))
			case None => Seq(token)
		}
	}

	/** Finds the closest next occurrence of any delimiter from the set. */
	private def findNext(str: String, delimiters: Set[Char]): Option[Int] = {
		val indices = delimiters map { c => str.indexOf(c) } filterNot {_ < 0}
		if (indices.isEmpty) None else Option(indices.min)
	}

	/**
	 * Removes non-alphanumeric characters (will drop parts of tokens after non-allowed characters), and shifts the
	 * token to lowercase.
	 * @return if there are several alphanumeric blocks in the token or if there are none returns [[None]], else the
	 *         token with only alphanumeric characters
	 */
	def strictTrimToken(token: Token): Option[Token] = {
		val (str, index) = token
		val matches = (cleanPattern findAllIn str.toLowerCase).toSeq
		if (matches.size == 1) Option((matches.head, index)) else None
	}
}
