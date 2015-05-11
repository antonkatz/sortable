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
import com.sortable.challenge.matching.{ReconciliationUtils, MatchComputations}
import com.sortable.challenge.{SimpleLogger => Log}

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try
/**
 * Ties together high level functions at the entry point.
 */
object Main {
	def main(args: Array[String]): Unit = {
		if (args.length != 2) {
			Log.error("Arguments should be: [products path] [listings path]")
			throw new IllegalArgumentException
		}

		val productPath: String = args(0)
		val listingPath: String = args(1)
		val data = loadDataFromFiles(productPath, listingPath) orElse {
			Log.error("Could not load the data from files.")
			None
		}

		/*  Find matches. Could remove listings as matches are found (for performance),
		but that would complicate quality analysis of the algorithm. */
//		val matches = data map {d => findMatches(d._1, d._2)}
//		val matches = data map {d => findMatches(Set(d._1.toSeq.slice(20, 30).find{_.name.contains("930")} get), d._2)}
		val matches = data map {d => ReconciliationUtils.findMatches(d._1 take 30 drop 20, d._2)}
		matches

		// check if any of the listings have been matched twice, indicating a bad algorithm
	}

	private[challenge] def loadDataFromFiles(productPath: String, listingPath: String): Option[(Set[Product], Set[Listing])] = {
		val productContents = Source.fromFile(productPath).getLines()
		val listingContents = Source.fromFile(listingPath).getLines()
		val products = convertToProducts(productContents.toSeq)
		val listings = convertToListings(listingContents.toSeq)

		products map { p => listings map { l => (p, l) } } flatten
	}
}
