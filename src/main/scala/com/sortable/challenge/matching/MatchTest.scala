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

import com.sortable.challenge.matching.TokenMatchType._
import com.sortable.challenge.matching.MatchingUtils._
import com.sortable.challenge.{Listing, Product}

import scala.language.postfixOps

/**
 * Tests whether a product and a listing are a match.
 */
case class MatchTest(product: Product, listing: Listing) {
	private var allTokenMatches = matchTokens(product.getNameTokens, listing.title, nameToTitle) ++
			matchTokens(product.getManufacturerTokens, listing.title, manufacturerToTitle) ++
			matchTokens(product.getModelTokens, listing.title, modelToTitle) ++
			matchTokens(product.getManufacturerTokens, listing.manufacturer, manufacturerToManufacturer)
	// family might not be present
	product.getFamilyTokens map {tokens =>
		matchTokens(tokens, listing.title, familyToTitle)} foreach {allTokenMatches ++= _}
	private val groupedMatches = allTokenMatches groupBy(_._1)

	private val clusters = groupedMatches mapValues MatchingUtils.findTightestCluster
	private val orderChanges = clusters mapValues MatchingUtils.getOrderChangeAroundPivot
	private val orderTally = {orderChanges mapValues {_ map (o => Math.pow(o._2, 2)) sum} values}.toSeq sum

	private val countMissing = allTokenMatches.size - product.getTotalTokenCount
	
	/**
	 * Whether this tests was successful (the product and the listing are a match).
	 */
	def isMatch: Boolean = true
}
