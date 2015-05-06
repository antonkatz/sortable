package com.sortable.challenge.matching

/**
 * Example usage: marking which product attribute a token came from and to which listing attribute it was matched.
 */
object TokenMatchType extends Enumeration {
  type TokenMatchType = Value
  val nameToTitle, manufacturerToTitle, modelToTitle, familyToTitle, manufacturerToManufacturer = Value
}
