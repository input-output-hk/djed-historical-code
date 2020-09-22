package stablecoin

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

  private def maxReserve(sc: N): N = maxReserveRatio * sc * oracle.conversionRate(peg, base)

  private def minReserve(sc: N): N = minReserveRatio * sc * oracle.conversionRate(peg, base)

  // The reservecoin's nominal price is its book value 
  // (i.e. the equity divided by the number of reservecoins in circulation).
  // Alternatively, we could have taken into account the present value of expected future fee revenues
  // or the market price of reservecoins provided by the oracle.
  private def reservecoinNominalPrice(r: N, sc: N, rc: N): N = {
    if (rc != 0) equity(r, sc)/rc else reservecoinDefaultPrice
  }

  private def stablecoinNominalPrice(r: N, sc: N): N = {
    val p = oracle.conversionRate(peg, base)
    if (sc == 0) p else min(p, liabilities(r, sc)/sc)
  }

  // ## General Functions
  // All functions here are total and side-effect free,
  // but they are not referentially transparent, because they depend on the bank's mutable state

  // There are two conditions for the acceptability of a reserve change:
  //  * If we are minting stablecoins or redeeming reservecoins, the new reserves shouldn't drop below the minimum.
  //  * If we are minting reservecoins, the new reserves shouldn't rise above the maximum.
  // Note that the new reserves can go above the maximum when stablecoins are being redeemed.
  // This ensures that stablecoin holders can always redeem their stablecoins. The only effect on
  // reservecoin holders when the reserves rise above the maximum is a reduction of the leverage of
  // the reservecoins in relation to the base currency.
  def acceptableReserveChange(mintsSC: Boolean,
                              mintsRC: Boolean,
                              redeemsRC: Boolean,
                              r: N, sc: N): Boolean = {
    def implies(a: Boolean, b: Boolean) = !a || b
    implies((mintsSC || redeemsRC), (r >= minReserve(sc))) && implies(mintsRC, (r <= maxReserve(sc)))
  }


  // A transaction that mints/redeems `amountSC` and `amountRC`
  // and withdraws/deposits `amountBase` in/from the bank
  // is valid if and only if all of the following hold:
  //    * the change in the reserves is acceptable
  //    * the price paid per reservecoin is the current nominal price
  //    * the price paid per stablecoin is the current nominal price
  //    * the fee paid is correct
  //
  // Note that a positive value means minting (in the case of `amountSC` and `amountRC`)
  // or withdrawing (in the case of `amountBase`) and a negative value means redeeming or depositing (respectively)
  def isValidTransaction(amountBase: N, amountSC: N, amountRC: N, feee: N): Boolean = {
    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val rcValueInBase = amountRC * reservecoinNominalPrice(reserves, stablecoins, reservecoins)

    val newReserves = reserves - amountBase + feee
    val newStablecoins = stablecoins + amountSC

    val correctPrices = { scValueInBase + rcValueInBase + amountBase == 0 }
    val correctFee = { feee == (abs(scValueInBase) + abs(rcValueInBase)) * fee }

    acceptableReserveChange(amountSC > 0, amountRC > 0, amountRC < 0, newReserves, newStablecoins) && correctPrices && correctFee
  }

  // Given amounts of stablecoins and reservecoins that one wants to mint (if positive) or redeem (if negative),
  // this function calculates how much one should withdraw (of positive) or deposit in the base currency and the fee
  // that must be paid in base currency.
  def mintOrRedeem(amountSC: N, amountRC: N): Option[(N,N)] = {
    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val rcValueInBase = amountRC * reservecoinNominalPrice(reserves, stablecoins, reservecoins)

    val amountBase = - (scValueInBase + rcValueInBase)
    val feee = (abs(scValueInBase) + abs(rcValueInBase)) * fee

    val newReserves = reserves - amountBase + feee
    val newStablecoins = stablecoins + amountSC

    if (acceptableReserveChange(amountSC > 0, amountRC > 0, amountRC < 0, newReserves, newStablecoins)) {
      Some((amountBase, feee))
    }
    else None
  } ensuring { _ match {
    case Some((amountBase, feeInBase)) => isValidTransaction(amountBase, amountSC, amountRC, feeInBase)
    case None => true
  }}
}

// Dividend payments, bonds and other financial products could be added as additional features.

// The "Bank" could be generalized so that a single "bank" could:
//   * issue more than one stablecoin
//   * have reserves in more than one base cryptocurrency
