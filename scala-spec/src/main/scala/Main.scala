
import scala.collection.mutable.{Map => MMap}
import scala.math.{min, max}

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello world!")
    println(msg)
  }

  def msg = "I was compiled by dotty :)"

}

type Currency = String
type Stablecoin = Currency
type Cryptocurrency = Currency
type Address = Int

class Oracle {
  private val m = MMap[(Currency,Currency), BigDecimal]()
  
  def conversionRate(from: Currency, to: Currency): BigDecimal = m((from, to))
  
  def updateConversionRate(from: Currency, to: Currency, rate: BigDecimal) = { m((from, to)) = rate }
}

case class Transaction(from: Address, to: Address, amount: BigDecimal, currency: Currency)

// This is just a mock ledger. Therefore, adding a transaction does nothing
class Ledger {
  def addTransactions(ts: Iterable[Transaction]) = {}
}

// The idea:
// The bank has a reserve and issues stablecoins when users demand it
// The bank buys back stablecoins when users want to sell them
// The bank profits (and its reserve grows) by trading the stablecoins with users using a spread
// The bank is only allowed to issue stablecoins if the reserves are above a minimum threshold
// The bank has shareholders.
// Shareholders contribute to the reserves.
// Shareholders receive dividends when the reserves are above a maximum threshold.

class Bank(address: Address, // bank's address 
           oracle: Oracle, // Oracle used by the bank
           ledger: Ledger, // Ledger in which the bank participates
           spread: BigDecimal, // the spread practiced by the bank when buying and selling stablecoins
           minReserveLevel: BigDecimal, // the bank's minimum reserve level
           maxReserveLevel: BigDecimal, // the bank's maximum reserve level 
           base: Cryptocurrency, // the base cryptocurrency used by the bank (e.g. ERG or ADA)
           share: Cryptocurrency, // shares issued by the bank to shareholders 
           stablecoin: Stablecoin, // the stablecoin issued and managed by the bank
           peg: Currency // the currency to which the stablecoin is pegged
          ) {
  
  // The bank's reserves in the base cryptocurrency (e.g. ERG or ADA)
  private var reserve: BigDecimal = 0
  
  // The amount of Stablecoins currently in circulation
  private var stablecoins: BigDecimal = 0
  
  // The addresses that hold shares of the bank and how many shares they hold
  private val shareholders = MMap[Address, BigDecimal]()
  
  private def shares() = shareholders.values.fold(BigDecimal(0))(_ + _)
  
  // The amount of reserves that would have to be paid if 
  // all stablecoin holders decided to sell their stablecoins 
  private def obligations() = max(reserve.toDouble, (stablecoins * oracle.conversionRate(peg, base) * (1 - spread/2)).toDouble)
  
  def reserveLevel(res: BigDecimal, sc: BigDecimal) = ((res * oracle.conversionRate(base, peg)) / sc)
  
  def buyStablecoin(amountBase: BigDecimal, buyerAddress: Address) = {
    val amountStablecoin = amountBase * oracle.conversionRate(base, peg) * (1 - spread/2) 
    if (reserveLevel(reserve + amountBase, stablecoins + amountStablecoin) > minReserveLevel) {
      stablecoins = stablecoins + amountStablecoin // issue desired amount of stablecoins
      reserve = reserve + amountBase
      val transferStablecoinsToBuyer = Transaction(address, buyerAddress, amountStablecoin, stablecoin)
      val transferBaseToBank = Transaction(buyerAddress, address, amountBase, base)
      ledger.addTransactions(List(transferStablecoinsToBuyer, transferBaseToBank))
    }
  }

  def sellStablecoin(amountStablecoin: BigDecimal, sellerAddress: Address) = {
    val rl = reserveLevel(reserve, stablecoins)
    
    val amountBase = amountStablecoin * oracle.conversionRate(peg, base) * (1 - spread/2) * min(1, rl.toDouble)
    // note the "min(1, rl)" factor covers the case when the reserves are not sufficient
    // to cover the redemption of all stablecoins in circulation (i.e. reserve level is below 1).
    // in this case, the peg is gracefully and temporarily abandoned, to ensure that 
    // all stablecoin holders have the right to an equal share of the remaining reserves.
    
    reserve = reserve - amountBase
    val transferBaseToSeller = Transaction(address, sellerAddress, amountBase, base)
    val transferStablecoinsToBank = Transaction(sellerAddress, address, amountStablecoin, stablecoin)
    stablecoins = stablecoins - amountStablecoin // burn received stablecoins
    ledger.addTransactions(List(transferStablecoinsToBank, transferBaseToSeller))
  }
  
  def buyShares(amountBase: BigDecimal, buyerAddress: Address) = {
    val r = reserve
    val o = obligations()
    // amount of shares that the buyer will get, calculated so that the book value per share remains the same
    val amountShares = shares() * (((r + amountBase - o) / (r - o)) - 1) * (1 - spread/2)
    
    // TODO: the fair price for a share should take into account not only the book value 
    // TODO: but also the present value of all expected future dividend payments
    // TODO: we could also take the oracle into account and sell shares for a price
    // TODO: that is the maximum of the fair price and the market price
    
    // FIXME: check that this doesn't bring the value of reserves above the max reserve level
    
    reserve = reserve + amountBase
    val transferBaseToBank = Transaction(buyerAddress, address, amountBase, base)
    shareholders(buyerAddress) = shareholders.getOrElse(buyerAddress, BigDecimal(0)) + amountShares
    
    ledger.addTransactions(List(transferBaseToBank))
  }
  
  
  
  def sellShares(amountShares: BigDecimal, sellerAddress: Address) = {
    val r = reserve
    val o = obligations()
    val amountBase = amountShares * (r - o)/shares() * (1 - spread/2)
    
    // TODO: the same comments from `buyShares` apply here as well
    
    // FIXME: check that the sale doesn't bring reserves below accceptable level
    
    reserve = reserve - amountBase
    val transferBaseToSeller = Transaction(address, sellerAddress, amountBase, base)
    shareholders(sellerAddress) = shareholders.getOrElse(sellerAddress, BigDecimal(0)) - amountShares

    ledger.addTransactions(List(transferBaseToSeller))
  }
  
  // When the reserves exceed the maximum reserve by more than 10%, 
  // dividends are paid to the shareholders to bring the reserve
  def payDividends() = {
    val maxReserve = maxReserveLevel * stablecoins * oracle.conversionRate(peg, base)
    if (reserve > 1.1 * maxReserve) { // FIXME: magic number
      val dividendPerShare = (reserve - maxReserve) / shares()
      val dividendTransactions = shareholders.map((shareholderAddress, amount) => Transaction(address, shareholderAddress, dividendPerShare * amount, base)).toList
      reserve = maxReserve
      ledger.addTransactions(dividendTransactions)
    }
    
    // FIXME: pay enough dividends to bring reserve to "(min + max)/2"
  }
}

// TODO: this could be extended with bonds and other financial products later.
// TODO: other financial products could include a "bear" coin, a "bull" coin, CFDs...
// TODO: this could be generalized so that a single "bank" could issue
// TODO: more than one stablecoin and have reserves in more than one base cryptocurrency.
// TODO: we could also consider dividend reinvestment plans