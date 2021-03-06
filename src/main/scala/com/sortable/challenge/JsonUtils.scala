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

import java.math.{MathContext, RoundingMode}

import play.api.libs.json._

import scala.language.postfixOps
import scala.util.Try

/**
 * Converts JSON data into internal representations of the same data.
 */
object JsonUtils {
  private val PRODUCT_NAME = "product_name"

  private val LISTING_TITLE = "title"

  private val LISTING_MANUFACTURER = "manufacturer"

  private val LISTING_CURRENCY = "currency"

  private val LISTING_PRICE = "price"

  /**
   * Checks that JSON is a valid product representation, and then creates [[Product]] instances.
   * @param jsonLines a collections of strings, each representing a product
   * @return if all JSON objects that supposed to represent products are valid returns a sequence of all those objects
   *         converted into [[Product]]. If a single JSON object is invalid, returns [[None]]
   */
  private[challenge] def convertToProducts(jsonLines: Seq[String]): Option[Seq[Product]] =
    conversionIterator(jsonLines, { productJson: JsValue =>
      val args = Seq(productJson \ PRODUCT_NAME, productJson \ "manufacturer", productJson \ "model")
          .filterNot(isInvalidJsonInst)
          .map(_.as[String])
      // sometimes is missing in JSON
      val family = Option(productJson \ "family") filterNot isInvalidJsonInst map { _.as[String] }
      if (args.length == 3) {
        Option(new Product(args(0), args(1), args(2), family))
      } else {
        None
      }
    })


  /**
   * Checks that JSON is a valid listing representation, and then creates [[Listing]] instances.
   * @param jsonLines a collections of strings, each representing a listing
   * @return if all JSON objects that supposed to represent listings are valid returns a sequence of all those objects
   *         converted into [[Listing]]. If a single JSON object is invalid, returns [[None]]
   */
  private[challenge] def convertToListings(jsonLines: Seq[String]): Option[Seq[Listing]] =
    conversionIterator(jsonLines, { listingJson: JsValue =>
      val args = Seq(listingJson \ LISTING_TITLE, listingJson \ LISTING_MANUFACTURER, listingJson \ LISTING_CURRENCY)
          .filterNot(isInvalidJsonInst)
          .map(_.as[String])
      Option(listingJson \ LISTING_PRICE) collect {
        case price: JsNumber if args.length == 3 => new Listing(args(0), args(1), args(2), price.value)
        case price: JsString if Try(price.value.toDouble).isSuccess && args.length == 3 =>
          new Listing(args(0), args(1), args(2), price.value.toDouble)
      }
    })

  /** Partial function for checking if an [[JsValue]] holds actual data. */
  private def isInvalidJsonInst: PartialFunction[JsValue, Boolean] = {
    case _: JsUndefined | JsNull => true
    case _ => false
  }

  /**
   * Behaviour shared by both product and listing conversions from JSON. Iterates over the lines (ex. from a file) and
   * applies a converter function to each individual JSON object.
   * @param jsonLines a collections of strings, each string representing an entity (listing or product)
   * @param converter which does the processing of an individual JSON entry into an object
   * @tparam H type of [[DataHolder]] that the converter function produces
   * @return a sequence of instances of [[DataHolder]] if all conversions were successful; otherwise [[None]]
   */
  private def conversionIterator[H <: DataHolder](jsonLines: Seq[String], converter: (JsValue) => Option[H]):
  Option[Seq[H]] = {
    var hasInvalid = false
    val results: Seq[Option[H]] = jsonLines map { entryString =>
      Json.parse(entryString) match {
        case entryJson: JsObject if !hasInvalid => converter(entryJson) match {
          case r: Some[H] => r
          case _ =>
            hasInvalid = true
            None
        }
        case _ => None
      }
    }
    if (hasInvalid) None else Option(results.flatten)
  }

  /** @return a [[JsObject]] representing results for a product. Contains the product's name and
    *         an array of listings. */
  private[challenge] def createResultJsonString(product: Product, listings: Iterable[Listing]): String = {
    val obj = Json.obj(PRODUCT_NAME -> JsString(product.name), "listings" -> { listings map listingToJson })
    Json.stringify(obj)
  }


  /* Could just store the original string in the listing class but that would be ugly */
  /** @return a [[JsObject]] representing a listing. */
  private def listingToJson(listing: Listing): JsObject = {
    val price = "%.2f" format listing.price
    Json.obj(LISTING_TITLE -> listing.originalTitle, LISTING_MANUFACTURER -> listing.originalManufacturer,
      LISTING_CURRENCY -> listing.currency, LISTING_PRICE -> price)
  }
}
