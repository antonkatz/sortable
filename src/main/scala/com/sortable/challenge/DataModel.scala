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
case class Product(name: String, manufacturer: String, model: String, family: Option[String]) extends DataHolder {
  private val delimiters = Set(' ', '_', '-', '/')

  private val nameTokens: Seq[Token] = tokenize(name, delimiters)
  private val manufacturerTokens: Seq[Token] = tokenize(manufacturer, delimiters)
  private val modelTokens: Seq[Token] = tokenize(model, delimiters)
  private val familyTokens: Option[Seq[Token]] = family map { f => tokenize(f, delimiters) }

  def getNameTokens = nameTokens

  def getManufacturerTokens = manufacturerTokens

  def getModelTokens = modelTokens

  def getFamilyTokens = familyTokens getOrElse Seq()
}

// used double instead of BigDecimal for performance
case class Listing(var title: String, var manufacturer: String, currency: String, price: Double) extends DataHolder {
  val originalTitle = title
  title = title.toLowerCase
  manufacturer = manufacturer.toLowerCase

  val adjustedPrice = PriceConverter.convert(price, currency)
}

trait DataHolder {
  /** Wrapper around [[com.sortable.challenge.TokenizationUtils.tokenizeWithIndex]] that logs a message if there are
    * problems. */
  def tokenize(str: String, delimiters: Set[Char]): Seq[Token] = {
    val tokens = tokenizeWithIndex(str, delimiters) map strictTrimToken
    if (tokens.contains(None)) {
      debug("Was not able to clean some of the tokens: %s", str)
    }
    tokens.flatten
  }
}

