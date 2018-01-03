import scala.util.DynamicVariable


trait Subscriber {
  def handler(pub: Publisher)
}


trait Publisher {
  private var subscribers: Set[Subscriber] = Set()

  def subscribe(subscriber: Subscriber): Unit = {
    subscribers += subscriber
  }

  def unsubscribe(subscriber: Subscriber): Unit = {
    subscribers -= subscriber
  }

  def publish(): Unit = {
    subscribers foreach(_.handler(this))
  }
}


class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance = balance

  def deposit(ammount: Int): Unit = {
    if (ammount > 0) balance += ammount
    publish()
  }

  def withdraw(amount: Int): Unit = {
    if (0 < amount && amount <= balance) {
      balance -= amount
      publish()
    } else throw new Error("insufficient funds")
  }
}


class BankAccount1 extends Publisher {
  //val balance = Var(0)

  def currentBalance = balance

  def deposit(ammount: Int): Unit = {
    if (ammount > 0) balance += ammount
    publish()
  }

  def withdraw(amount: Int): Unit = {
    if (0 < amount && amount <= balance) {
      balance -= amount
      publish()
    } else throw new Error("insufficient funds")
  }
}


class Consolidator(observed: List[BankAccount]) extends Subscriber {
  private var total:Int = _

  def totalBalance = total

  private def compute(): Unit =
    total = observed.map(_.currentBalance).sum

  override def handler(pub: Publisher): Unit = compute()

  observed.foreach(_.subscribe(this))
  compute()
}
