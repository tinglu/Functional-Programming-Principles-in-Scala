import math.abs


def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  }

  loop(a, 0)
}
sum(x => x * x, 3, 5)




def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))


def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
//def product(f: Int => Int)(a: Int, b: Int): Int = {
//  if (a > b) 1
//  else f(a) * product(f)(a + 1, b)
//}
product(x => x * x)(3, 4)


def fact(n: Int): Int = product(x => x)(1, n)
fact(6)


// Fixed Point

val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) = abs((x - y) / x) / x < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    println(guess)
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }

  iterate(firstGuess)
}
fixedPoint(x => 1 + x / 2)(1)


// sqrt(x) is a fixed point of the function (y => x / y)
def sqrt(x: Double) =
  fixedPoint(y => (y + x / y) / 2)(1.0)
sqrt(2)
sqrt(4)


// the iteration converges by averaging succesive values y => (y + x / y) / 2
// this technique of stabilizing by averaging is a general enough to merit being abstracted into its own function
def averageDamp(f: Double => Double)(y: Double) =
  (y + f(y)) / 2

def sqrt2(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1)
sqrt2(2)
