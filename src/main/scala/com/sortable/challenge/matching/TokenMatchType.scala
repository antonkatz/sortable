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
import com.sortable.challenge.{Listing, Product}

/**
 * @usecase marking which product attribute a token came from and to which listing attribute it was matched.
 */
object TokenMatchType extends Enumeration {
  type TokenMatchType = Value
  val nameToTitle, manufacturerToTitle, modelToTitle, familyToTitle, manufacturerToManufacturer = Value

  /** @return the listing's attribute based on the token type. */
  private[matching] def getDestination(matchType: TokenMatchType, listing: Listing): String = matchType match {
    case this.nameToTitle | this.manufacturerToTitle | this.modelToTitle | this.familyToTitle => listing.title
    case this.manufacturerToManufacturer => listing.manufacturer
  }

  private def getTokens(matchType: TokenMatchType, product: Product): Iterable[Token] = matchType match {
    case this.nameToTitle => product.getNameTokens
    case this.manufacturerToTitle | this.manufacturerToManufacturer => product.getManufacturerTokens
    case this.modelToTitle => product.getModelTokens
    case this.familyToTitle => product.getFamilyTokens
  }

  /** @return a tuple consisting of a collection of tokens and a string, which are retrieved from the product and the
    *         listing respectively, based on type. The tokens come from the product, and the string is one of the
    *         listing's attributes. */
  def getConstituents(matchType: TokenMatchType, product: Product, listing: Listing): (Iterable[Token], String) = {
    (getTokens(matchType, product), getDestination(matchType, listing))
  }
}
