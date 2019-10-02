object session {
  println("Welcome to the Scala worksheet")


}

//println("Welcome to the Scala worksheet")
//1+2

//import session._
//
//sqrt(2)
//sqrt(4)
//sqrt(1e-6)
//sqrt(1e60)


def abs(x: Double) = if (x < 0) -x else x

def isGoodEnough(guess: Double, x: Double) =
  abs(guess * guess - x) / x < 0.001

def improve(guess: Double, x: Double) =
  (guess + x / guess) / 2

def sqrtIter(guess: Double, x: Double): Double =
  if (isGoodEnough(guess, x)) guess
  else sqrtIter(improve(guess, x), x)



def sqrt(x: Double) =
  sqrtIter(1.0, x)


sqrt(2)
sqrt(4)
sqrt(1e-6)
sqrt(1e60)