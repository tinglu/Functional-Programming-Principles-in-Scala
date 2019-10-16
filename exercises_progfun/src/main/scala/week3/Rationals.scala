package week3


// Functions and Data
class Rational(x: Int, y: Int) { // primart constructor
  require(y != 0, "denominator must be nonzero")

  def this(x: Int) = this(x, 1) // second constructor

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def number = x / g

  def denom = y / g

  // If numer and denom are called infrequently
  //  def number = x / gcd(x, y)
  //  def denom = y / gcd(x, y)
  // If numer and denom are called often
  //  val number = x / gcd(x, y)
  //  val denom = y / gcd(x, y)

  //  def less(that: Rational) =
  //    this.number * that.denom < that.number * this.denom
  def <(that: Rational) =
    this.number * that.denom < that.number * this.denom

  //  def max(that: Rational) =
  //    if (this.less(that)) that else this
  def max(that: Rational) =
    if (this < that) that else this

  //  def add(that: Rational) =
  //    new Rational(
  //      number * that.denom + that.number * denom,
  //      denom * that.denom)
  def +(that: Rational) =
    new Rational(
      number * that.denom + that.number * denom,
      denom * that.denom)

  //  def neg = new Rational(-number, denom)
  def unary_- : Rational = new Rational(-number, denom)

  //  def sub(that: Rational) =
  //    new Rational(
  //      number * that.denom - that.number * denom,
  //      denom * that.denom)
  //  def sub(that: Rational) = add(that.neg)
  def -(that: Rational) = this + -that

  //  def mul(that: Rational) =
  //    new Rational(
  //      number * that.number,
  //      denom * that.denom)
  def *(that: Rational) =
    new Rational(
      number * that.number,
      denom * that.denom)

  override def toString: String = number + "/" + denom
}
