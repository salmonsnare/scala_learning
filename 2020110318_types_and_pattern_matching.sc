// 2020110318
// 型とパターンマッチング
// 参考: https://www.coursera.org/learn/progfun1/home/week/4

// Lecture 4.6
// case classes
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr

/*
object Number {
  def apply(n: Int) = new Number(n)
}
object Sum {
  def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
}
*/

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1, e2) => eval(e1) + eval(e2)
}

object exprs {
  def show(e: Expr): String = e match {
    case Number(x) => x.toString                // 数値を文字列に変換
    case Sum(l, r) => show(l) + " + " + show(r) // 文字列を連接
  }
}
exprs.show(Sum(Number(1), Number(44)))

// Lecture 4.7
// list
var fruit = List("apples", "oranges", "pears")
val nums = List(1, 2, 3, 4)
val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val empty = List()

var fruit: List[String] = List("apples", "oranges", "pears")
val nums: List[Integer] = List(1, 2, 3, 4)
val diag3: List[List[Integer]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
val empty: List[Nothing] = List()

// 空リストは Nil で記す。
// construction operation :: (cons)
// x :: xs は新しいリストを与える。これは、最初の要素として x をもち、xs の要素が続く。
var fruit = "apples" :: ("oranges" :: ("pears" :: Nil))
var nums = 1 :: (2 :: (3 :: (4 :: Nil)))
var empty = Nil

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
}

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

isort(List(0, 3, 1, 2))