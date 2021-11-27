import Data.Graph
import cats.{Applicative, Monoid}
import cats.effect._
import cats.syntax.all._

import java.io.{FileReader, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.io.StdIn.{readLine => readLn}
import scala.util.{Success, Try}
object Data {
  trait Orientation
  case object Forward extends Orientation
  case object Backward extends Orientation

  case class Chain[A](begin: A, path: List[(Edge[A], Orientation)]) {
    def increment(grph: Graph[A]): Graph[A] = {
      path.foldLeft(grph) {
        case (grph, (Edge(from, to, _, f), Forward)) => grph.setF(from, to, f + h)
        case (grph, (Edge(from, to, _, f), Backward)) => grph.setF(from, to, f - h)
      }
    }
    def concat(other: Chain[A]): Chain[A] = Chain(begin, path ::: other.path)
    def add(other: Edge[A], orientation: Orientation): Chain[A] = Chain(begin, (other, orientation) :: path)
    def h: Int = path.map {
      case (edge, Forward) => edge.capacity - edge.f
      case (edge, Backward) => edge.f
    }.min
  }

  case class Edge[A](from: A, to: A, capacity: Int, f: Int)

  case class Graph[A](core: Set[Edge[A]]) {
    def vertices: Set[A] = core.map(_.from) ++ core.map(_.to)

    def outEdges(from: A): List[Edge[A]] = core.filter(_.from == from).toList

    def inEdges(to: A): List[Edge[A]] = core.filter(_.to == to).toList

    def setF(from: A, to: A, f: Int): Graph[A] = {
      val maybeEdge = core.find(c => c.from == from && c.to == to)
      maybeEdge match {
        case Some(value) => Graph(core.filterNot(_ == value) + Edge(from, to, value.capacity, f))
        case None => this
      }
    }

    def getChains(s: A, t: A): List[Chain[A]] = {
      def getChainsHelper(s: A, t: A, was: Chain[A]): List[Chain[A]] = {
        val (outList, inList) = (
          outEdges(s).filterNot(was.path.map(_._1).contains).filterNot(_.capacity == 0),
          inEdges(s).filterNot(was.path.map(_._1).contains).filterNot(_.capacity == 0)
        )
        val outChains = for {
          out <- outList
          futureOut <- getChainsHelper(out.to, t, was.add(out, Forward))
        } yield futureOut

        val inChains = for {
          in <- inList
          futureIn <- getChainsHelper(in.from, t, was.add(in, Backward))
        } yield futureIn

        was :: outChains ::: inChains
      }

      getChainsHelper(s, t, Chain(s, Nil)).filter(
        c => c.path match {
          case (head, _) :: _ if head.to == t || head.from == t => true
          case _ => false
        }
      )
    }

    def findAddChain(s: A, t: A): Option[Chain[A]] = getChains(s, t).find(_.h > 0)

    def buildMaximalFlow(s: A, t: A, init: Int = 0): (Graph[A], Int) = {
      val maybeChain = findAddChain(s, t)
      maybeChain.fold((this, init))(
        chain => chain.increment(this).buildMaximalFlow(s, t, chain.h + init)
      )
    }

    def flowMatrix(implicit ordered: Ordering[A]): String = {
      vertices.toList.sorted
        .map(
          v => outEdges(v).sortBy(_.to).map(_.f).mkString(" ")
        ).mkString("\n")
    }

  }

  implicit def monoidGraph[A]: Monoid[Graph[A]] = new Monoid[Graph[A]] {
    override def empty: Graph[A] = Graph[A](Set.empty[Edge[A]])

    override def combine(x: Graph[A], y: Graph[A]): Graph[A] =
      Graph(x.core ++ y.core)
  }
  object Graph {
    def apply[A](from: A, to: A, capacity: Int): Graph[A] = capacity match {
      case i => Graph(Set(Edge(from, to, capacity, 0)))
    }
  }
}

trait Reader[F[_]] {
  def readLine: F[String]
}
trait Printer[F[_]] {
  def printLine(s: String): F[Unit]
}

object Main extends IOApp {
  class Program(printer: Printer[IO], reader: Reader[IO]) {
    def start(): IO[Unit] =
      for {
        count <- reader.readLine.map(_.toInt)
        rows: List[String] <- List.range(0, count).as(
          reader.readLine
        ).sequence
        graphs = for {
          (data, row) <- rows.zipWithIndex
          (capacity, col) <- data.split(" ").zipWithIndex.toList
        } yield Graph(row, col, capacity.toInt)
        graph = graphs.toNel.get.reduce
        start <- reader.readLine.map(_.toInt - 1)
        end <- reader.readLine.map(_.toInt - 1)
        (way, max) = graph.buildMaximalFlow(start, end)
        _ <- printer.printLine(way.flowMatrix)
        _ <- printer.printLine(max.toString)

      } yield ()
  }

  implicit class ApplicativeOps[F[_]: Applicative, A](a: F[A]) {
    def *[B](b: F[B]): F[(A, B)] = Applicative[F].product(a, b)
  }


  def withReader(r: Reader[IO] => IO[Unit]): IO[Unit] = {
    import scala.collection.mutable.{Queue => MQ}
    val q: MQ[String] = MQ()
    val source = Source.fromFile("input.txt")

    source.mkString.split("\n").foreach(q.enqueue)
    val reader =  new Reader[IO] {
      override def readLine: IO[String] =
        IO(q.dequeue()).handleError(_ => "")
    }
    val result = r(reader)
    source.close()
    result
  }
  def withPrinter(p: Printer[IO] => IO[Unit]): IO[Unit] = {
    val printWriter = new PrintWriter("out.txt")
    val printer = new Printer[IO] {
      override def printLine(s: String): IO[Unit] = IO(printWriter.println(s))
    }
    val result = p(printer)
    printWriter.close()
    result
  }



  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- withPrinter(p => withReader(r => new Program(p, r).start()))
    } yield ExitCode.Success



  def decartian[F[_]: Applicative, A](a: F[A]): F[(A, A)] = Applicative[F].product(a, a)

}
