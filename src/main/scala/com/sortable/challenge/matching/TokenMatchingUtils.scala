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

import scala.language.postfixOps

/**
 * Methods for finding positions of tokens across attributes
 */
object TokenMatchingUtils {
  /** Type, position in the original attribute, position in the destination (searched in) attribute. */
  type TokenMatch = (TokenMatchType, Int, Int)

  /**
   * Finds all positions of a sequence of tokens inside a destination string.
   * @param matchType example usage: indicating which attribute (name, manufacturer, etc.) a token came from and
   *                  which attribute (title, manufacturer, etc.) it was matched to
   * @return a sequence of [[TokenMatch]]es that have been found. A single [[Token]] can have any number
   *         [[TokenMatch]]es (including 0)
   */
  def doMatching(tokens: Seq[Token], destination: String, matchType: TokenMatchType): Seq[TokenMatch] = {
    val all: Seq[TokenMatch] = tokens map { t =>
      val (tokenStr, index) = t
      findAllWithIndex(tokenStr, destination) map { destIndex => (matchType, index, destIndex) }
    } flatten;
    all filterNot {
      _._3 == -1
    }
  }

  /** Finds all positions of a string in another string. */
  private def findAllWithIndex(what: String, in: String): Seq[Int] = in indexOf what match {
    case -1 => Seq()
    case pos => pos +: findAllWithIndex(what, in substring pos + 1)
  }
}
