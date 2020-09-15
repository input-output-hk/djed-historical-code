
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

// The bank keeps a reserve of an underlying base cryptocurrency 
// The bank issues and redeems stablecoins and reservecoins
// 
// The stablecoin is pegged to a peg currency
// The exchange rate between the peg currency and the base cryptocurrency is provided by an oracle
//
// The holder of a stablecoin has a claim to a variable portion of the reserves according to the exchange rate
//
// The bank's equity is the bank's reserves minus the bank's liabilities to the stablecoin holders
// The bank's equity is owned by the reservecoin holders
//
// The bank profits (and its reserve grows) by trading the stablecoins and reservecoins using a spread
// 
// The bank's reserve ratio is the bank's reserves divided by the bank's liabilities
// The bank is only allowed to issue and sell stablecoins if the reserve ratio remains above a minimum threshold
// 
// To avoid dilution (https://en.wikipedia.org/wiki/Stock_dilution), the bank is only allowed 
// to issue and sell reservecoins if the reserve ratio remains below a maximum threshold


class MinimalBank(protected val address: Address, // bank's address 
           protected val oracle: Oracle, // Oracle used by the bank
           protected val ledger: Ledger, // Ledger in which the bank participates
           fee: BigDecimal, // the fee charged by the bank when buying and selling stablecoins and reservecoins
           minReserveRatio: BigDecimal, // the bank's minimum reserve ratio
           protected val maxReserveRatio: BigDecimal, // the bank's maximum reserve ratio 
           protected val base: Cryptocurrency, // the base cryptocurrency used by the bank (e.g. ERG or ADA)
           reservecoin: Cryptocurrency, // shares issued by the bank to shareholders
           stablecoin: Stablecoin, // the stablecoin issued and managed by the bank
           protected val peg: Currency // the currency to which the stablecoin is pegged
          ) {
  
  // The bank's reserves in the base cryptocurrency (e.g. ERG or ADA)
  protected var reserve: BigDecimal = 0
  
  // The amount of stablecoins currently in circulation
  protected var stablecoins: BigDecimal = 0
  
  // The amount of reservecoins currently in circulation
  protected var reservecoins: BigDecimal = 0
  
  // The amount of reserves that would have to be paid if 
  // all stablecoin holders decided to sell their stablecoins 
  private def liabilities() = min(reserve.toDouble, (stablecoins * oracle.conversionRate(peg, base) * (1 - fee)).toDouble)
  
  def reserveRatio(res: BigDecimal, sc: BigDecimal) = ((res * oracle.conversionRate(base, peg)) / sc)
  
  def buyStablecoin(amountBase: BigDecimal, buyerAddress: Address) = {
    val amountStablecoin = amountBase * oracle.conversionRate(base, peg) * (1 - fee) 
    if (reserveRatio(reserve + amountBase, stablecoins + amountStablecoin) > minReserveRatio) {
      stablecoins = stablecoins + amountStablecoin // issue desired amount of stablecoins
      reserve = reserve + amountBase
      val transferStablecoinsToBuyer = Transaction(address, buyerAddress, amountStablecoin, stablecoin)
      val transferBaseToBank = Transaction(buyerAddress, address, amountBase, base)
      ledger.addTransactions(List(transferStablecoinsToBuyer, transferBaseToBank))
    }
  }

  def sellStablecoin(amountStablecoin: BigDecimal, sellerAddress: Address) = {
    val rl = reserveRatio(reserve, stablecoins)
    
    val amountBase = amountStablecoin * oracle.conversionRate(peg, base) * (1 - fee) * min(1, rl.toDouble)
    // the "min(1, rl)" factor covers the case when the reserves are not sufficient
    // to cover the redemption of all stablecoins in circulation (i.e. reserve ratio is below 1).
    // in this case, the peg is gracefully and temporarily abandoned, to ensure that 
    // all stablecoin holders have the right to an equal share of the remaining reserves.
    
    reserve = reserve - amountBase
    val transferBaseToSeller = Transaction(address, sellerAddress, amountBase, base)
    val transferStablecoinsToBank = Transaction(sellerAddress, address, amountStablecoin, stablecoin)
    stablecoins = stablecoins - amountStablecoin // burn received stablecoins
    ledger.addTransactions(List(transferStablecoinsToBank, transferBaseToSeller))
  }
  
  def buyReservecoin(amountBase: BigDecimal, buyerAddress: Address) = {
    val r = reserve
    val o = liabilities()
    // amount of shares that the buyer will get, calculated so that the book value per share remains the same
    val amountShares = reservecoins * (((r + amountBase - o) / (r - o)) - 1) * (1 - fee)
    
    // FIXME: check that this doesn't bring the value of reserves above the max reserve ratio
    
    reserve = reserve + amountBase
    val transferBaseToBank = Transaction(buyerAddress, address, amountBase, base)
    reservecoins = reservecoins + amountShares
    
    ledger.addTransactions(List(transferBaseToBank))
  }
  
  def sellReservecoin(amountShares: BigDecimal, sellerAddress: Address) = {
    val r = reserve
    val o = liabilities()
    val amountBase = amountShares * (r - o)/reservecoins * (1 - fee)
    
    // FIXME: check that the sale doesn't bring reserves below accceptable ratio
    
    reserve = reserve - amountBase
    val transferBaseToSeller = Transaction(address, sellerAddress, amountBase, base)
    reservecoins = reservecoins - amountShares

    ledger.addTransactions(List(transferBaseToSeller))
  }
}

// Dividend payments, bonds and other financial products could be added as additional features.

// The "Bank" could be generalized so that a single "bank" could:
//   * issue more than one stablecoin
//   * have reserves in more than one base cryptocurrency

// The fair price for a reservecoin could be not the book value 
// but could also take into account the present value of all expected future dividend payments
// It could also take the oracle into account and sell reservecoins for a price
// that is the maximum of the fair price and the market price