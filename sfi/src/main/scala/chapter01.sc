
val a = math.sqrt(3)
val b = math.pow(a, 2)

"crazy" * 3 // https://www.scala-lang.org/api/current/scala/collection/immutable/StringOps.html#*(n:Int):String

10 max 2 // RichInt

BigInt(2) pow 1024


import scala.util.Random
import scala.BigInt._
probablePrime(1000, Random)

val c = BigInt(Random.nextInt())
c.toString(36)

val d = "abcdefg"
d.head
d.last

d.take(3)
d.drop(3)
d.takeRight(3)
d.dropRight(3)
d.substring(3, 4)