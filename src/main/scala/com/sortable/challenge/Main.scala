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

import com.sortable.challenge.JsonUtils.{convertToListings, convertToProducts}
import com.sortable.challenge.matching.MatchTest
import com.sortable.challenge.{SimpleLogger => log}

import scala.io.Source
import scala.language.postfixOps

/**
 * Ties together high level functions at the entry point.
 */
object Main {
	def main(args: Array[String]) = {
		if (args.length != 2) {
			log.error("Arguments should be: [products path] [listings path]")
			throw new IllegalArgumentException
		}

		val productPath: String = args(0)
		val listingPath: String = args(1)
		val data = loadDataFromFiles(productPath, listingPath) orElse {
			log.error("Could not load the data from files.")
			None
		}

		/*  Find matches.
		Could remove listings as matches are found, but that would complicate quality analysis of the algorithm. */



		// check if any of the listings have been matched twice, indicating a bad algorithm
	}

	private def loadDataFromFiles(productPath: String, listingPath: String): Option[(Set[Product], Set[Listing])] = {
		val productContents = Source.fromFile(productPath).getLines()
		val listingContents = Source.fromFile(listingPath).getLines()
		val products = convertToProducts(productContents.toSeq)
		val listings = convertToListings(listingContents.toSeq)

		products map { p => listings map { l => (p, l) } } flatten
	}

	private def findMatches(products: Set[Product], listings: Set[Listing]): Map[Product, Set[MatchTest]] =
		products map { p => p -> findMatches(p, listings) } toMap

	/** Finds all listings that match a particular product. */
	private def findMatches(product: Product, listings: Set[Listing]): Set[MatchTest] = {
		// using fold to reduce the number of times the seq has to be rebuilt
		listings.foldLeft {
			Set[MatchTest]()
		} { (seq: Set[MatchTest], listing: Listing) =>
			val test = new MatchTest(product, listing)
			if (test.isMatch) seq + test else seq
		}
	}
}
