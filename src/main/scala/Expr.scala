object Expr extends App {

  import matryoshka._, implicits._, data._
  import scalaz._

  sealed abstract class Expr[A]
  final case class Num[A](value: Long) extends Expr[A]
  final case class Mul[A](l: A, r: A)  extends Expr[A]
  final case class Plus[A](l: A, r: A) extends Expr[A]


  implicit val exprFunctor = new scalaz.Functor[Expr] {
    override def map[A, B](fa: Expr[A])(f: (A) => B) = fa match{
      case Num(value) => Num[B](value)
      case Mul(l, r) => Mul(f(l), f(r))
      case Plus(l, r) => Plus(f(l), f(r))
    }
  }

  val eval: Algebra[Expr, Long] = { // i.e. Expr[Long] => Long
    case Num(x)    => x
    case Mul(x, y) =>  x * y
    case Plus(x, y) =>  x + y
  }

  val show: Algebra[Expr, String] = {
    case Num(x) => s"$x"
    case Mul(x, y) => s"($x * $y)"
    case Plus(x, y) => s"$x + $y"
  }

  val pluses: Coalgebra[Expr, Long] = x => {
    if(x>1)  Plus(1, x-1) else Num(1)
  }

  val generate = 12L.ana[Mu[Expr]](pluses)
  println(generate.cata(show))
  println(hylo(12L)(eval,pluses))


  val fact: GCoalgebra[\/[Fix[Expr], ?], Expr, Long] = x => {
    if(x>1)  Mul(-\/(Fix(Num(x))), \/-(x-1)) else Num(1)
  }

  val expFact = 12L.apo[Fix[Expr]](fact)

  println(expFact.cata(show))
  println(expFact.cata(eval))
}
