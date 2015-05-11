package com.sortable.challenge

/**
 * Converts prices in different currencies into relative units.
 */
object PriceConverter {
  private val conversionMap = Map("CAD" -> 1.0, "USD" -> 1.2, "EUR" -> 1.4, "GBP" -> 1.9)

  def convert(price: Double, currency: String): Option[Double] = {
    conversionMap get(currency toUpperCase) map(_ * price) orElse {
      SimpleLogger.warn("Currency not found: %s", currency)
      None
    }
  }
}
