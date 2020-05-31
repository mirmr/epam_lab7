import scala.annotation.tailrec

object Main {

  def count(in: List[Char], c: Char): Int = in match {
    case a :: rest if a == c => 1 + count(rest, c)
    case _ :: rest => count(rest, c)
    case Nil => 0
  }

  @scala.annotation.tailrec
  def append[A](in: List[A], a: A, c: Int): List[A] = c match {
    case x if x <= 0 => in
    case x => append[A](in.appended(a), a, x - 1)
  }

  @scala.annotation.tailrec
  def lappend[A](in: List[A], a: A, c: Int): List[A] = c match {
    case x if x <= 0 => in
    case x => lappend(a :: in, a, x - 1)
  }

  @tailrec
  def foldl[A, B](f: (B, A) => B, a: B, c: List[A]): B = c match {
    case Nil => a
    case r :: rest => foldl(f, f(a, r), rest)
  }

  def equalize_parentheses_core(in: List[Char]): List[Char] = count(in, '(') - count(in, ')') match {
    case x if x < 0 => lappend[Char](in, '(', -x)
    case x if x > 0 => append[Char](in, ')', x)
    case 0 => in
  }

  def equalize_parentheses(in: String): String =
    foldl[Char, String]((str, c) => str.appended(c), "", equalize_parentheses_core(in.toList))


  def main(args: Array[String]): Unit = {
    println(equalize_parentheses("(hell(o))world)"))
    println(equalize_parentheses("(((hell(o))world)"))
    println(equalize_parentheses("((hell(o))world)"))
    println(equalize_parentheses("(hell(o))world))"))
    println(equalize_parentheses("(hell(o))worl()d)"))
    println(equalize_parentheses("(hell(o))wor())ld)"))
    println(equalize_parentheses("(((((hell(o))wor())ld)"))
  }
}
