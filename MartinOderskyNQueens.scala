/** 
 * The N-Queen solution by Martin Odersky presented in his course 
 * Funcitonal Programming Principles in Scala. Link to the video:
 * https://www.coursera.org/lecture/scala-functional-programming/lecture-6-3-combinatorial-search-example-H3cKk
**/

def isSafe(col: Int, queens: List[Int]): Boolean =
  def checks(col: Int, delta: Int, queens: List[Int]): Boolean = queens match
    case qcol :: others =>
      qcol == col                       // vertical check
      || (qcol - col).abs == delta      // diagonal check
      || checks(col, delta + 1, others)
    case Nil =>
      false
  !checks(col, 1, queens)

def queens(n: Int) =
  def placeQueens(k: Int): Set[List[Int]] =
    if k == 0 then Set(List())
    else
      for
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      yield col :: queens
  placeQueens(n)
