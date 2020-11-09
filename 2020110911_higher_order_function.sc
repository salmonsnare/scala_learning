// 2020110911
// 高階関数の勉強
// 参考: https://www.coursera.org/learn/progfun1/home/week/2

def sum(f: Int => Int, a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int =
    if (a > b) acc
    else loop(a + 1, f(a) + acc)
  loop(a, 0)
}

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
  if (a > b) zero
  else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))

// 引数 a と b の積の 2 乗
def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

// (2 * 3)^2 = 36
product(x => x * x)(2, 3)

// (3 * 4)^2 = 144
product(x => x * x)(3, 4)

// 階乗
def fact(n: Int) = product(x => x)(1, n)
fact(5)

// sum_{x in {a, ..b}} x^2 を算出。
// 3 * 3 + 4 * 4 + 5 * 5 = 9 + 16 + 25 = 50
sum(x => x * x, 3, 5)