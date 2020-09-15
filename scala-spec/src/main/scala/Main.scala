
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
type N = BigDecimal

def min(x: N, y: N) = if (x < y) x else y
def max(x: N, y: N) = if (x > y) x else y
def abs(x: N) = if (x >= 0) x else -x

class Oracle {
  private val m = MMap[(Currency,Currency), N]()
  
  def conversionRate(from: Currency, to: Currency): N = m((from, to))
  
  def updateConversionRate(from: Currency, to: Currency, rate: N) = { m((from, to)) = rate }
}

case class Transaction(from: Address, to: Address, amount: N, currency: Currency)

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
// The bank profits (and its reserve grows) by minting and redeeming the stablecoins and reservecoins for a fee
// 
// The bank's reserve ratio is the bank's reserves divided by the amount of stablecoins in circulation
// The bank is only allowed to issue and sell stablecoins and reservecoins if the reserve ratio 
// remains above a minimum threshold and below a maximum threshold. 
// The maximum threshold prevents dilution (https://en.wikipedia.org/wiki/Stock_dilution) for reservecoin holders. 
// The minimum threshold aims to ensure that stablecoins remain fully backed by reserves 
// even if the price of the base currency falls.


class MinimalBank(address: Address,   // bank's address 
                  oracle: Oracle,     // Oracle used by the bank
                  ledger: Ledger,     // Ledger in which the bank participates
                  fee: N,  // the fee charged by the bank when buying and selling stablecoins and reservecoins
                  minReserveRatio: N, // the bank's minimum reserve ratio
                  maxReserveRatio: N, // the bank's maximum reserve ratio 
                  base: Cryptocurrency,   // the base cryptocurrency used by the bank
                  peg: Currency,          // the currency to which the stablecoin is pegged
                  stablecoin: Stablecoin, // the stablecoin issued and managed by the bank
                  reservecoin: Cryptocurrency, // the reservecoins that represent ownership of the bank's equity
                  reservecoinDefaultPrice: N // default price of reservecoins, used when there are 0 reservecoins
                 ) {
  
  // ## Bank's State
  private var reserves: N = 0     // The bank's reserves in the base cryptocurrency
  private var stablecoins: N = 0  // The amount of stablecoins currently in circulation
  private var reservecoins: N = 0 // The amount of reservecoins currently in circulation
  
  // ## Auxiliary Functions
  // All functions here are total, side-effect-free and referentially transparent
  
  private def liabilities(r: N, sc: N): N = {
    min(r, sc * oracle.conversionRate(peg, base))
  } ensuring { _ <= r}

  private def equity(r: N, sc: N): N = {
    r - liabilities(r, sc)
  } ensuring { _ >= 0 }  
  
  private def reserveRatio(r: N, sc: N): N = {
    require(sc != 0)
    ((r * oracle.conversionRate(base, peg)) / sc)
  }
  
  private def isReserveRatioAcceptable(r: N, sc: N): Boolean = {
    if (sc == 0) false
    else {
      val ratio = reserveRatio(r, sc)
      ratio <= maxReserveRatio && ratio >= minReserveRatio
    }
  }
  
  private def reservecoinNominalPrice(r: N, sc: N, rc: N): N = {
    if (rc != 0) equity(r, sc)/rc
    else reservecoinDefaultPrice
  } 
  
  private def stablecoinNominalPrice(r: N, sc: N): N = {
    val p = oracle.conversionRate(peg, base)
    if (sc == 0) p
    else min(p, liabilities(r, sc)/sc) 
  }
  
  // ## General Functions
  // All functions here are total and side-effect free,
  // but they are not referentially transparent, because they depend on the bank's mutable state
  
  // A transaction that mints/redeems `amountSC` and `amountRC` 
  // and withdraws/deposits `amountBase` in/from the bank
  // is valid if and only if all of the following hold:
  //    * the reserve ratio remains acceptable
  //    * the price paid per reservecoin is the current nominal price
  //    * the price paid per stablecoin is the current nominal price
  //    * the fee paid is correct
  //
  // Note that a positive value means minting (in the case of `amountSC` and `amountRC`) 
  // or withdrawing (in the case of `amountBase`) and a negative value means redeeming or depositing (respectively)
  def isValidTransaction(amountBase: N, amountSC: N, amountRC: N, feeInBase: N): Boolean = {
    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val rcValueInBase = amountRC * reservecoinNominalPrice(reserves, stablecoins, reservecoins)

    val correctRatio = isReserveRatioAcceptable(reserves - amountBase + feeInBase, stablecoins + amountSC)
    val correctPrices = { scValueInBase + rcValueInBase + amountBase == 0 }
    val correctFee = { feeInBase == (abs(amountBase) + abs(scValueInBase) + abs(rcValueInBase)) * fee }
    
    correctRatio && correctPrices && correctFee
  }
  
  // Given amounts of stablecoins and reservecoins that one wants to mint (if positive) or redeem (if negative), 
  // this function calculates how much one should withdraw (of positive) or deposit in the base currency and the fee
  // that must be paid in base currency.
  def mintOrRedeem(amountSC: N, amountRC: N): Option[(N,N)] = {
    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val rcValueInBase = amountRC * reservecoinNominalPrice(reserves, stablecoins, reservecoins)
    
    val amountBase = - (scValueInBase + rcValueInBase)
    val feee = (abs(amountBase) + abs(scValueInBase) + abs(rcValueInBase)) * fee
    
    if (isReserveRatioAcceptable(reserves - amountBase + feee, stablecoins + amountSC)) Some((amountBase, feee))
    else None
  } ensuring { _ match {
    case Some((amountBase, feeInBase)) => isValidTransaction(amountBase, amountSC, amountRC, feeInBase)
  }}
  
  
//  // ## Convenient Special Case Functions
//  
//  
//  def buyStablecoin(amountBaseAndFee: N): Option[N] = {
//    val amountBase = amountBaseAndFee * (1 - (2 * fee))
//    val amountSC = 
//    
//    
//    val amountStablecoin = amountBase * oracle.conversionRate(base, peg) * (1 - fee) 
//    if (isReserveRatioAcceptable(reserves + amountBase, stablecoins + amountStablecoin)) {
//      stablecoins = stablecoins + amountStablecoin // issue desired amount of stablecoins
//      reserves = reserves + amountBase
//      val transferStablecoinsToBuyer = Transaction(address, buyerAddress, amountStablecoin, stablecoin)
//      val transferBaseToBank = Transaction(buyerAddress, address, amountBase, base)
//      ledger.addTransactions(List(transferStablecoinsToBuyer, transferBaseToBank))
//    }
//  }
//
//  def sellStablecoin(amountStablecoin: N, sellerAddress: Address) = {
//    val rl = reserveRatio(reserves, stablecoins)
//    
//    val amountBase = amountStablecoin * oracle.conversionRate(peg, base) * (1 - fee) * min(1, rl.toDouble)
//    // the "min(1, rl)" factor covers the case when the reserves are not sufficient
//    // to cover the redemption of all stablecoins in circulation (i.e. reserve ratio is below 1).
//    // in this case, the peg is gracefully and temporarily abandoned, to ensure that 
//    // all stablecoin holders have the right to an equal share of the remaining reserves.
//    
//    reserves = reserves - amountBase
//    val transferBaseToSeller = Transaction(address, sellerAddress, amountBase, base)
//    val transferStablecoinsToBank = Transaction(sellerAddress, address, amountStablecoin, stablecoin)
//    stablecoins = stablecoins - amountStablecoin // burn received stablecoins
//    ledger.addTransactions(List(transferStablecoinsToBank, transferBaseToSeller))
//  }
//  
//  def buyReservecoin(amountBase: N, buyerAddress: Address) = {
//    val r = reserves
//    val o = liabilities(reserves, stablecoins)
//    // amount of shares that the buyer will get, calculated so that the book value per share remains the same
//    val amountShares = reservecoins * (((r + amountBase - o) / (r - o)) - 1) * (1 - fee)
//    
//    // FIXME: check that this doesn't bring the value of reserves above the max reserve ratio
//    
//    reserves = reserves + amountBase
//    val transferBaseToBank = Transaction(buyerAddress, address, amountBase, base)
//    reservecoins = reservecoins + amountShares
//    
//    ledger.addTransactions(List(transferBaseToBank))
//  }
//  
//  def sellReservecoin(amountShares: N, sellerAddress: Address) = {
//    val r = reserves
//    val o = liabilities(r, stablecoins)
//    val amountBase = amountShares * (r - o)/reservecoins * (1 - fee)
//    
//    // FIXME: check that the sale doesn't bring reserves below accceptable ratio
//    
//    reserves = reserves - amountBase
//    val transferBaseToSeller = Transaction(address, sellerAddress, amountBase, base)
//    reservecoins = reservecoins - amountShares
//
//    ledger.addTransactions(List(transferBaseToSeller))
//  }
}

// Dividend payments, bonds and other financial products could be added as additional features.

// The "Bank" could be generalized so that a single "bank" could:
//   * issue more than one stablecoin
//   * have reserves in more than one base cryptocurrency

// The fair price for a reservecoin could be not only the book value 
// but could also take into account the present value of expected future fee revenue
// It could also take the oracle into account and sell reservecoins for a price
// that is the maximum of the fair price and the market price