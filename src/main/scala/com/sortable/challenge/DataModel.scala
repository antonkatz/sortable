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

import com.sortable.challenge.SimpleLogger.debug
import com.sortable.challenge.TokenizationUtils._

/**
 * Case classes for convenient representation of underlying data.
 */
private[challenge] case class Product(name: String, manufacturer: String, model: String, family: Option[String])
    extends DataHolder {
  /** Which are used to split the attributes/strings into tokens. */
  private val delimiters = Set(' ', '_', '-', '/', '(', ')')

  private val nameTokens: Seq[Token] = tokenize(name, delimiters)
  private val manufacturerTokens: Seq[Token] = tokenize(manufacturer, delimiters)
  private val modelTokens: Seq[Token] = tokenize(model, delimiters)
  private val familyTokens: Option[Seq[Token]] = family map { f => tokenize(f, delimiters) }

  private[challenge] def getNameTokens = nameTokens

  private[challenge] def getManufacturerTokens = manufacturerTokens

  private[challenge] def getModelTokens = modelTokens

  private[challenge] def getFamilyTokens = familyTokens getOrElse Seq()
}

private[challenge] case class Listing(var title: String, var manufacturer: String, currency: String, price: BigDecimal)
    extends DataHolder {
  /** The original string of the listing, before any processing. */
  private[challenge] val originalTitle = title
  private[challenge] val originalManufacturer = manufacturer
  title = title.toLowerCase
  manufacturer = manufacturer.toLowerCase

  /** Price adjusted for difference in currency values. In other words this is the absolute price used by the
    * algorithm. */
  private[challenge] val adjustedPrice = PriceConverter.convert(price.toDouble, currency)
}

private[challenge] trait DataHolder {
  /**
   * Wrapper around [[com.sortable.challenge.TokenizationUtils.tokenizeWithIndex]] that logs a message if there are
   * problems.
   */
  protected def tokenize(str: String, delimiters: Set[Char]): Seq[Token] = {
    val tokens = tokenizeWithIndex(str, delimiters) map strictCleanToken
    if (tokens.contains(None)) {
      debug("Was not able to clean some of the tokens: %s", str)
    }
    tokens.flatten
  }
}

