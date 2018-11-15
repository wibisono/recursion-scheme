import matryoshka._, implicits._, data._
import scalaz._

object Sudoku extends App  {
  val N = 3
  sealed trait Cell
  case class Fill(value : Int) extends Cell
  case class Slot(possible : List[Int]) extends Cell

  val empty = Slot( (1 to N).toList)

  type Cells = Array[Cell]


  sealed abstract class Board[A]
  case class Solved[A]( rows : Cells) extends Board[A]
  case class Choice[A](child : List[A]) extends Board[A]

 implicit val boardFunctor: Functor[Board] = new scalaz.Functor[Board] {
    override def map[A, B](fa: Board[A])(f: (A) => B): Board[B] = fa match {
      case Solved(rows) => Solved[B](rows)
      case Choice(childs) => Choice(childs.map(f))
    }
  }

val show: Algebra[Board, String] = {
  case Solved(cells) => cells.sliding(N,N).map(_.mkString(" ")).mkString("\n")
  case Choice(childs) => childs.mkString("\n--|--\n")
}

val gen: Coalgebra[Board, Cells] = rows => {
   Solved(rows)
}

val init: Array[Cell] = (1 to 9).toArray.map(_=>empty)


println(hylo(init)(show, gen))



}