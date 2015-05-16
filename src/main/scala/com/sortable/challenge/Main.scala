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
import com.sortable.challenge.matching.Algorithm
import com.sortable.challenge.{SimpleLogger => Log}

import scala.io.Source
import scala.language.postfixOps

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

    /*  Find matches. Could remove listings as matches are found (for accuracy and possibly performance),
    but that would complicate the algorithm beyond the scope of this project */
    val matches = data map { d => Algorithm.findMatches(d._1 take 30 drop 20, d._2) }
    matches

    /* Should check if any of the listings have been matched twice indicating problems, but will opt out not to */
  }

  private[challenge] def loadDataFromFiles(productPath: String, listingPath: String): Option[(Seq[Product],
      Seq[Listing])] = {
    val productContents = Source.fromFile(productPath).getLines() toSeq
    val listingContents = Source.fromFile(listingPath).getLines() toSeq
    /* These are not sets because some are identical */
    val products = convertToProducts(productContents)
    val listings = convertToListings(listingContents)

    products map { p => listings map { l =>
      loadWarning(productContents, p, "products")
      loadWarning(listingContents, l, "listings")
      (p, l)
    }
    } flatten
  }

  private def loadWarning[T](lines: Iterable[String], items: Iterable[T], what: String) =
    if (lines.count(_ nonEmpty) != items.size) {
      val msg = "The number of %1$s loaded does not match the number of %1$s available".format(what)
      Log.warn(msg)
    }
}
