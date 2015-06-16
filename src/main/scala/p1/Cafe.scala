package p1

class Cafe {
  def buyCoffee(cc:CreditCard): Coffee = {
    val cup = new Coffee()
    cc.charge(cup.price)
    cup
  }
}

case class CreditCard() {
  def charge(c:Int):Unit = {}
}

case class Coffee() {
  var price:Int = 0
}
