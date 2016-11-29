def sum1(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if(a > b) acc
    else loop(a+1, f(a)+acc)
  }
  loop(a, 0)
}

def sum(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 0
  else f(a) + sum(f)(a+1, b)
}

def product(f: Int => Int)(a: Int, b: Int): Int = {
  if(a > b) 1
  else f(a) * product(f)(a+1, b)
}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
}

def sumGeneral(f: Int => Int)(a: Int, b: Int): Int = mapReduce(x => x, (a, b) => a+b, 0)(a, b)
def sumProduct(a: Int, b: Int): Int = mapReduce(a => a, (a, b) => a*b, 1)(a, b)

def factorial(n: Int) = product(x => x)(1, n)


sum(x => x)(1, 4)
product(x => x * x)(3, 7)
sum1(x => x)(1, 4)
factorial(3)
