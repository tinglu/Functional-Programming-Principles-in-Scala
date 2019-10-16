import week3._

object Hello {
  def main(args: Array[String]) = println("Hello World")


  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException
    else if (n == 0) xs.head
    else nth(n - 1, xs.tail)

  val list = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil[Int])))
  println(nth(2, list))
  println(nth(4, list))
  println(nth(-1, list))
}
