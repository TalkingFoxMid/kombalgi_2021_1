import cats.Applicative
import cats.effect._
import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.StdIn

case class Dot(x: Int, y: Int, id: Int)
case class Edge(d1: Dot, d2: Dot, length: Double)

trait Reader[F[_]] {
  def readLine: F[String]
}
trait Printer[F[_]] {
  def printLine(s: String): F[Unit]
}

case class DSU[A](pred: A => A) {
  def origin(a: A): A = if (a == pred(a)) a else origin(pred(a))
  def isTogether(a: A, b: A): Boolean = origin(a) == origin(b)
  def union(a: A, b: A): DSU[A] = DSU[A](el => if (el == origin(a)) b else pred(el))
}

object DSU {
  def empty[A]: DSU[A] = DSU(identity[A])
}
object Main extends IOApp {

  implicit class ApplicativeOps[F[_]: Applicative, A](a: F[A]) {
    def *[B](b: F[B]): F[(A, B)] = Applicative[F].product(a, b)
  }

  implicit val reader: Reader[IO] = new Reader[IO] {
    override def readLine: IO[String] = IO(StdIn.readLine())
  }

  implicit val printer: Printer[IO] = (s: String) => IO(println(s))

  override def run(args: List[String]): IO[ExitCode] =
    for {
      count <- reader.readLine.map(_.toInt)
      dots <- readDots[IO](count, 1)
      edges = decartian(dots).map { case (d1, d2) => Edge(d1, d2, distance(d1, d2))}
        .filterNot(edge => edge.d1 == edge.d2)
        .sortBy(_.length)
      ostov = constructOstov(edges, Nil, DSU.empty[Edge])
      result = dots.map(dot => ostov.map(edge => List(edge.d1, edge.d2)).collect {
        case List(a, b) if a == dot => b
        case List(a, b) if b == dot => a
      })
      _ <- result.traverse {
        lst => printer.printLine(lst.map(_.id).appended(0).mkString(" "))
      }
      _ <- printer.printLine(ostov.map(_.length).sum.toString)
    } yield ExitCode.Success

  @tailrec
  def constructOstov(freez: List[Edge], claimed: List[Edge], dsu: DSU[Edge]): List[Edge] = freez match {
    case head :: freezTail => {
      val left: Option[Edge] = claimed.find(edge => edge.d1 == head.d1 || edge.d2 == head.d1)
      val right: Option[Edge] = claimed.find(edge => edge.d1 == head.d2 || edge.d2 == head.d2)
      List(left, right).filter(_.isDefined) match {
        case List(Some(a), Some(b)) => {
          if (dsu.isTogether(a, b)) constructOstov(freezTail, claimed, dsu)
          else constructOstov(freezTail, head :: claimed, dsu.union(a, b).union(b, head))
        }
        case List(Some(a)) => constructOstov(freezTail, head :: claimed, dsu.union(a, head))
        case _ => constructOstov(freezTail, head :: claimed, dsu)
      }
    }
    case Nil => claimed
  }

  def readDots[F[_]: Applicative](count: Int, id: Int)(implicit reader: Reader[F]): F[List[Dot]] =

    if (count == 0) Applicative[F].pure(Nil) else (reader.readLine * readDots(count - 1, id + 1)).map {
      case (str, dots) => str.split(" ").map(_.toIntOption) match {
        case Array(Some(a), Some(b)) => Dot(a, b, id) :: dots
        case _ => ???
      }
    }

  def decartian[F[_]: Applicative, A](a: F[A]): F[(A, A)] = Applicative[F].product(a, a)

  def distance(d1: Dot, d2: Dot): Double = Math.sqrt {
    val dx = d1.x - d2.x
    val dy = d1.y - d2.y
    dx * dx + dy * dy
  }
}
