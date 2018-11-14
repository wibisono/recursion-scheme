import matryoshka._, implicits._, data._
import scalaz._

object Sudoku extends App  {


  type Row = Array[Int]
  type Rows = Array[Row]

  sealed abstract class Board[A]
  case class Solved[A]( rows : Rows) extends Board[A]
  case class Candidate[A]( childs : List[A]) extends Board[A]
  case class Init[A]( rows : Rows) extends Board[A]

 implicit val boardFunctor = new scalaz.Functor[Board] {
    override def map[A, B](fa: Board[A])(f: (A) => B) = fa match {
      case Solved(rows) => Solved[B](rows)
      case Init(rows) => Init[B](rows)
      case Candidate(childs) => Candidate(childs.map(f))
    }
  }

val show: Algebra[Board, String] = {
  case Solved(rows) => rows.map(_.mkString(" ")).mkString("\n")
  case Init(rows) => rows.map(_.mkString(" ")).mkString("\n")
  case Candidate(childs) => childs.mkString("\n--|--\n")
}

val gen: Coalgebra[Board, Rows] = rows => {
  if(rows.length<10) Candidate( List((1+:rows.head) +: rows))
    else Solved(rows)
}

val init = Array(Array(1,2,3))

println(hylo(init)(show, gen))

println(init(0)(0))

}