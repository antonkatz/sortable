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

/**
 * Converts prices in different currencies into absolute units.
 */
object PriceConverter {
  private val conversionMap = Map("CAD" -> 1.0, "USD" -> 1.2, "EUR" -> 1.4, "GBP" -> 1.9)

  /**
   * Converts prices to absolute units based on an internal map of currencies to conversions factors.
   * @return absolute price or [[None]] if the currency's conversion factor is unknown.
   */
  private[challenge] def convert(price: Double, currency: String): Option[Double] = {
    conversionMap get (currency toUpperCase) map (_ * price) orElse {
      SimpleLogger.warn("Currency not found: %s", currency)
      None
    }
  }
}
