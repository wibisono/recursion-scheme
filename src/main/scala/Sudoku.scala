import Expr.Expr
import matryoshka._
import implicits._
import data._
import scalaz._


object Sudoku extends App {
  val N = 3

  sealed trait Cell

  case class Fill(value: Int) extends Cell

  case class Slot(possible: List[Int]) extends Cell

  val initSlot: Cell = Slot((1 to N).toList)

  type Row = Array[Cell]
  type Rows = Array[Row]

  sealed abstract class Board[A]

  case class Single[A](cells: Rows) extends Board[A]

  case class Choice[A](child: List[A]) extends Board[A]

  implicit val boardFunctor: Functor[Board] = new scalaz.Functor[Board] {
    override def map[A, B](fa: Board[A])(f: (A) => B): Board[B] = fa match {
      case Single(rows) => Single[B](rows)
      case Choice(childs) => Choice(childs.map(f))
    }
  }

  val show: Algebra[Board, String] = {
    case Single(rows) => rows.map(_.mkString(" ")).mkString("\n")
    case Choice(childs) => childs.mkString("\n--|--\n")
  }

  def clearCell(v: Int) : Cell => Cell = {
    case Slot(pos) => Slot(pos.filterNot(_ == v))
    case Fill(v) => Fill(v)
  }

  def clearRow(v: Int) : Row => Row = { r => r map clearCell(v) }


  def clearCol(col: Int, v: Int, rows: Rows) = {
    val res = rows.clone()
    (0 until N).foreach { row =>
      res(row)(col) = clearCell(v)(res(row)(col))
    }
    res
  }

  def expand(rows: Rows) : List[Rows] = List(rows)

  type EitherFix[T] = \/[Fix[Board], T]

  val gen: Coalgebra[ Board, Rows] = rows => {
    val expanded = expand(rows)
    if(expanded.size == 1)
      Single(expanded.head)
    else
      Choice(expanded)
  }

  val init: Rows = (1 to N * N).toArray.sliding(N, N).map(_.map(_ => initSlot)).toArray

  val single = Single(init)
  init(1) = clearRow(2)(init(1))
  println(hylo(init)(show, gen))


}