// 2020110717
// コレクション
// 参考: https://www.coursera.org/learn/progfun1/home/week/6

// 配列
val xs = Array(1, 2, 3, 44)
xs map (x => x * 2) // 配列の値を 2 倍

val str = "Hello World"
str filter (c => c.isUpper) // 大文字のみ抽出
str exists (c => c.isUpper) // 大文字が存在するならば、true
str forall (c => c.isUpper) // すべて大文字ならば、frue

val pairs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) zip str
pairs.unzip

str flatMap (c => List('.', c))

// ベクター
val nums = Vector(1, 2, 3, -1)
val people = Vector("Bob", "James", "Peter")

// レンジ
val r: Range = 1 until 5
val s: Range = 1 to 5
1 to 10 by 1
10 to 1 by -1

// 素数かどうかを判定
def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

val n = 7
(1 until n) flatMap (i =>
  (1 until i) map (j => (i, j))) filter ( pair =>
    isPrime(pair._1 + pair._2))


def queens(n: Int): Set[List[Int]] = {
  def isSafe(col: Int , queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by - 1) zip queens
    queensWithRow forall {
      case (r, c) => col != c && math.abs(col - c) != row - r
    }
  }

  def placeQueens(k: Int): Set[List[Int]] =
    if (k == 0) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      } yield col :: queens
  placeQueens(n)
}

def show(queens: List[Int]) = {
  val lines =
    for(col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  "\n" + (lines mkString "\n")
}
(queens(4) map show) mkString "\n"

// 多項式のクラス, Int 型として次数をもち、Double 型として係数をもつ。
class Poly(terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
  def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff+"x^"+exp) mkString "+"
}

// 2.0 * x + 4.0 * x^3 + 6.2 * x^5
val p1 = new Poly(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))

// 3.0 + 7.0 * x^3
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 + p2

