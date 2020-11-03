// 2020110208
// リストの勉強
// 参考: https://www.coursera.org/learn/progfun1/home/week/5
// Lecture 5.1

def last[T](xs: List[T]): T = xs match {
  case List() => throw new Error("last of empty list")
  case List(x) => x
  case y :: ys => last(ys)
}

def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => List()
  case y :: ys => y :: init(ys)
}

def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
  case List() => ys
  case z :: zs => z :: concat(zs, ys)
}

def reverse[T](xs: List[T]): List[T] = xs match {
  case List() => xs
  case y :: ys => reverse(ys) ++ List(y)
}

def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n + 1)


val integer_list = List(1, 2, 3)
val rational_list = List(1.1, 2.2, 3.3)

init(integer_list)
last(integer_list)
init(rational_list)
last(rational_list)
concat(integer_list, rational_list)
removeAt(0: Int, integer_list)
removeAt(1: Int, integer_list)
removeAt(2: Int, integer_list)

// Lecture 5.2
object mergesort {
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  msort(nums)
}
