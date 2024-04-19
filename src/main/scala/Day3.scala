object Day3 extends App{
  def maximumProfit(priceofStocks : Array[Int]): Int = {
    if(priceofStocks.isEmpty || priceofStocks.length==1) {
      return 0
    }
    val (maximumProfit, _)= priceofStocks.tail.foldLeft((0,priceofStocks.head)) {
      case ((maximumProfit, minimumPrice), currentPrice) =>
        val currentProfit = currentPrice - minimumPrice
        val newMaximumProfit = Math.max(maximumProfit, currentProfit)
        val newMinimumProfit = Math.min(minimumPrice, currentPrice)
        (newMaximumProfit, newMinimumProfit)
    }
    maximumProfit
  }

  val prices = Array(7,1,5,3,6,4)
  val result = maximumProfit(prices)
  println("Maximum Profit = " + result)

}
