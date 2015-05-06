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

import com.sortable.challenge.matching.MatchTest

import scala.io.Source
import com.sortable.challenge.{SimpleLogger => log}
import JsonUtils.{convertToProducts, convertToListings}

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
  }

	// fixme change into iterables / iterators
  private def loadDataFromFiles(productPath: String, listingPath: String): Option[(Seq[Product], Seq[Listing])] = {
    val productContents = Source.fromFile(productPath).getLines()
    val listingContents = Source.fromFile(listingPath).getLines()
    val products = convertToProducts(productContents.toSeq)
    val listings = convertToListings(listingContents.toSeq)

    products map {p => listings map {l => (p, l)}} flatten
  }

	private def findMatches(products: Seq[Product], listings: Seq[Listing]): Map[Product, Seq[MatchTest]] =
		products map {p => p -> findMatches(p, listings)} toMap

	/** Finds all listings that match a particular product. */
  private def findMatches(product: Product, listing: Seq[Listing]): Seq[MatchTest] = {
		// using fold to reduce the number of times the seq has to be rebuilt
		listing foldLeft Seq[MatchTest]() {(seq, listing) => }
	}
}
