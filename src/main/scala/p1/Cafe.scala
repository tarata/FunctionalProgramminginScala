package p1

object Cafe {
  def coalesce(charges: Seq[Charge]): Seq[Charge] = charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toSeq
}

case class Charge(cc:CreditCard, amount:Double) {
  def combine(other:Charge): Charge =
    if(cc == other.cc) {
      copy(amount = amount + other.amount)
    } else {
      throw new Exception("Cannot combine charges to different cards")
    }
}

class Cafe {
  def buyCoffee(cc:CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc:CreditCard, n:Int): (Seq[Coffee], Charge) = {
    val purchases: Seq[(Coffee,Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
  }
}

case class CreditCard() {
  def charge(c:Int):Unit = {}
}

case class Coffee() {
  var price:Int = 0
}
