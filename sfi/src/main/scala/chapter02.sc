import java.time.LocalDate

def signmum(x: Int) = if (x > 0) 1 else if (x < 0) 1 else 0

{val x = 1; x}

var y: Int = 2
var x: Unit = y = 1

for (i <- 1 to 10) {println(i)}

def countdown(n : Int) {
  for (i <- n to 0 by -1) println(i)
}
countdown(10)

var sum: Long = 1
for (c <- "Hello") {
  sum = sum * c.toLong
}
sum

"Hello".map(_.toLong).product

def product(s: String) = s.map(_.toLong).product
product("Hello")

def product2(s: String): Long =
  if(s.isEmpty) 1.toLong
  else s.head.toLong * product2(s.tail)
product2("Hello")


def func(x: Int, n: Int): Int =
  if (n == 0) 1
  else if (n > 0) {
    if (x % 2 == 0)
     func(func(x, n / 2), 2)
    else
      x * func(x, n - 1)
  } else 1 / func(x, -n)

func(5, 1)

implicit class DataInterpolator(val sc: StringContext) extends AnyVal {
  def date(args: Any*): LocalDate = ???
}