package com.sortable.challenge.matching

import com.sortable.challenge.Product
import com.sortable.challenge.matching.TokenMatchingUtils.TokenMatch

/**
 * Example usage: marking which product attribute a token came from and to which listing attribute it was matched.
 */
object TokenMatchType extends Enumeration {
  type TokenMatchType = Value
  val nameToTitle, manufacturerToTitle, modelToTitle, familyToTitle, manufacturerToManufacturer = Value

  def getOrigin(matchType: TokenMatchType, product: Product): String = matchType match {
    case this.nameToTitle => product.name
    case this.manufacturerToTitle|this.manufacturerToManufacturer => product.manufacturer
    case this.modelToTitle => product.model
    case this.familyToTitle => product.family.getOrElse("")
  }
}
