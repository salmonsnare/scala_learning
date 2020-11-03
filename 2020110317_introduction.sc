// 2020110318
// Scala の文法の勉強
// 参考: https://www.coursera.org/learn/progfun1/home/week/1
// Lecture 1.5

object session {
  def abs(x: Double) = if (x < 0) -x else x

  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x) / x < 0.001

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)

  def sqrt(x: Double) = sqrtIter(1.0, x)

  sqrt(2)
}
session.sqrt(2)
session.sqrt(4)
session.sqrt(1e-6)
session.sqrt(1e60)