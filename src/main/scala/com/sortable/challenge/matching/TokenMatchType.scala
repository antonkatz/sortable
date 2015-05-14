package com.sortable.challenge.matching

import com.sortable.challenge.TokenizationUtils.Token
import com.sortable.challenge.{Listing, Product}
import com.sortable.challenge.matching.TokenMatchingUtils.TokenMatch

/**
 * Example usage: marking which product attribute a token came from and to which listing attribute it was matched.
 */
object TokenMatchType extends Enumeration {
  type TokenMatchType = Value
  val nameToTitle, manufacturerToTitle, modelToTitle, familyToTitle, manufacturerToManufacturer = Value

  def getDestination(matchType: TokenMatchType, listing: Listing): String = matchType match {
    case this.nameToTitle | this.manufacturerToTitle | this.modelToTitle | this.familyToTitle => listing.title
    case this.manufacturerToManufacturer => listing.manufacturer
  }

  def getOrigin(matchType: TokenMatchType, product: Product): Iterable[Token] = matchType match {
    case this.nameToTitle => product.getNameTokens
    case this.manufacturerToTitle|this.manufacturerToManufacturer => product.getManufacturerTokens
    case this.modelToTitle => product.getModelTokens
    case this.familyToTitle => product.getFamilyTokens
  }

  def getConstituents(matchType: TokenMatchType, product: Product, listing: Listing): (Iterable[Token], String) = {
    (getOrigin(matchType, product), getDestination(matchType, listing))
  }
}
