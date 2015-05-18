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

import java.io.FileWriter

import com.sortable.challenge.JsonUtils.{convertToListings, convertToProducts, createResultJsonString}
import com.sortable.challenge.matching.Algorithm
import com.sortable.challenge.{SimpleLogger => Log}

import scala.io.Source
import scala.language.postfixOps
import scala.util.Try

/**
 * Ties together high level functions at the entry point to the program.
 */
object Main {
  /** Takes path to products file and path to listings file as arguments. */
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

    Log.msg("Processing started")
    data map { d =>
      Algorithm.findMatches(d._1, d._2)
    } map { writeResults }
  }

  private[challenge] def loadDataFromFiles(productPath: String, listingPath: String): Option[(Seq[Product],
      Seq[Listing])] = {
    var productContents = Seq[String]()
    var listingContents = Seq[String]()
    val load = Try {
      productContents = Source.fromFile(productPath).getLines() toSeq;
      listingContents = Source.fromFile(listingPath).getLines() toSeq
    }
    if (load.isFailure) {
      load.failed map { th => Log.error(th, "Files could not be loaded") }
      return None
    }
    /* These are not sets because some are identical */
    val products = convertToProducts(productContents)
    val listings = convertToListings(listingContents)

    products flatMap { p => listings map { l =>
      loadWarning(productContents, p, "products")
      loadWarning(listingContents, l, "listings")
      (p, l)
    }
    }
  }

  private def writeResults(results: Map[Product, Iterable[Listing]]) = Try {
    val file = new FileWriter("results/results.txt")
    results foreach { r =>
      val json = createResultJsonString(r._1, r._2)
      file write json
      file write "\n"
    }
    file.flush()
    file.close()
  }.failed map { th => Log.error(th, "Could not write results to file") }

  private def loadWarning[T](lines: Iterable[String], items: Iterable[T], what: String) =
    if (lines.count(_ nonEmpty) != items.size) {
      val msg = "The number of %1$s loaded does not match the number of %1$s available".format(what)
      Log.warn(msg)
    }
}
