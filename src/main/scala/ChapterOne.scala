/**
  * Created by Kirill on 1/25/2017.
  */
object ChapterOne {

  case class Coffee(price: Int)

  case class CreditCard(number: Int)

  case class Charge(cc: CreditCard, amount: Double) {
    def combine(that: Charge) = {
      if (this.cc == that.cc)
        Charge(this.cc, this.amount + that.amount)
      else
        throw new Exception("Cannot combine different credit cards.")
    }
  }

  def coffeePrice = 12

  def buyCoffe(cc: CreditCard) = {
    val purchase = Coffee(coffeePrice)
    (purchase, Charge(cc, purchase.price))
  }

  def buyCoffes(cc: CreditCard, amount: Int) = {
    val purchases = List.fill(amount)(buyCoffe(cc))
    val (coffees, charges) = purchases unzip
    val finalCharge = charges reduce ((c1, c2) => c1 combine c2)
    (coffees, finalCharge)
  }

  def coalesce(variousCharges:List[Charge]) = variousCharges.groupBy(_.cc).values.map(oneList => oneList.reduce(_ combine _)) toList
}
