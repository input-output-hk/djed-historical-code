package stablecoin

case class Transaction(from: Address, to: Address, amount: N, currency: Currency)

// This is just a mock ledger. Therefore, adding a transaction does nothing
class Ledger {
  def addTransactions(ts: Iterable[Transaction]) = {}
}