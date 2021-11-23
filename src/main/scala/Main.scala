import Data.Graph
import cats.effect._
import cats.syntax.all._
import cats.{Applicative, Monoid}

import scala.io.StdIn.{readLine => readLn}

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

  case class Graph[A](g: Set[Edge[A]]) {
    def vertices: Set[A] = g.map(_.from) ++ g.map(_.to)

    def outEdges(from: A): List[Edge[A]] = g.filter(_.from == from).toList

    def inEdges(to: A): List[Edge[A]] = g.filter(_.to == to).toList

    def setF(from: A, to: A, f: Int): Graph[A] = {
      val maybeEdge = g.find(c => c.from == from && c.to == to)
      maybeEdge match {
        case Some(value) => Graph(g.filterNot(_ == value) + Edge(from, to, value.capacity, f))
        case None => this
      }
    }

    def getChains(s: A, t: A): List[Chain[A]] = {
      def getChainsHelper(s: A, t: A, was: Chain[A]): List[Chain[A]] = {
        val outList = outEdges(s).filterNot(was.path.map(_._1).contains).filter(_.capacity != 0)
        val inList = inEdges(s).filterNot(was.path.map(_._1).contains).filter(_.capacity != 0)

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

    def show[B](func: Edge[A] => B)(filter: A => Boolean)(implicit ordered: Ordering[A]): String =
      vertices
        .filter(filter)
        .toList
        .sorted
        .map(v => outEdges(v).filter(x => filter(x.to)))
        .collect {
          case v if v.nonEmpty => v.sortBy(_.to).map(func(_)).mkString(" ")
        }.mkString("\n")
  }

  implicit def monoidGraph[A]: Monoid[Graph[A]] = new Monoid[Graph[A]] {
    override def empty: Graph[A] = Graph[A](Set.empty[Edge[A]])

    override def combine(x: Graph[A], y: Graph[A]): Graph[A] =
      Graph(x.g ++ y.g)
  }

  object Graph {
    def apply[A](from: A, to: A, capacity: Int): Graph[A] = capacity match {
      case _ => Graph(Set(Edge(from, to, capacity, 0)))
    }
  }
}

trait Reader[F[_]] {
  def readLine: F[String]
}

trait Printer[F[_]] {
  def printLine(s: String): F[Unit]

  def printStr(s: String): F[Unit]
}

object Main extends IOApp {
  class Program(printer: Printer[IO], reader: Reader[IO]) {
    def start(): IO[Unit] = for {
      Array(rowsCount, colsCount) <- reader.readLine.map(_.split(" ").map(_.toInt))
      rows <- (0 until rowsCount).toList.traverse(_ => reader.readLine)
      (begin, end) = ("s", "t")
      edges = for {
        (row, rowName) <- rows.zip(LazyList.from(1).map(i => s"x$i"))
        (weight, colName) <- row.split(" ").zip(LazyList.from(1).map(i => s"y$i"))
        graph = Graph(rowName, colName, weight.toInt) |+| Graph(begin, rowName, 1) |+| Graph(colName, end, 1)
      } yield graph
      graph = edges.toNel.get.reduce
      filter = (v: String) => (v != end && v != begin)
//      _ <- printer.printLine("Graph: ")
//      _ <- printer.printLine(graph.show(_.capacity)(filter))

//      _ <- printer.printLine("Edges: ")
//      _ <- edges.traverse(_.g.toList.traverse(e => printer.printStr(s"(${e.from}, ${e.to}: ${e.capacity}) ")) >> printer.printLine(""))

//      _ <- printer.printLine("Flow: ")
      (flow, _) = graph.buildMaximalFlow(begin, end)
//      _ <- printer.printLine(flow.show(_.f)(filter))

//      _ <- printer.printLine("Pairs: ")
//      _ <- printer.printStr("[")
      pairs = flow.g.filter(_.f != 0).filter(e => filter(e.from) && filter(e.to))
      _ <- pairs.toList.traverse(e => printer.printStr(s"(${e.from}, ${e.to})"))
//      _ <- printer.printStr("]")
    } yield ()
  }

  implicit class ApplicativeOps[F[_] : Applicative, A](a: F[A]) {
    def *[B](b: F[B]): F[(A, B)] = Applicative[F].product(a, b)
  }


  def withReader(r: Reader[IO] => IO[Unit]): IO[Unit] = {
    val reader = new Reader[IO] {
      override def readLine: IO[String] = IO(readLn)
    }
    r(reader)
  }

  def withPrinter(p: Printer[IO] => IO[Unit]): IO[Unit] = {
    val printer = new Printer[IO] {
      override def printLine(s: String): IO[Unit] = IO(println(s))

      override def printStr(s: String): IO[Unit] = IO(print(s))
    }
    p(printer)
  }


  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- withPrinter(p => withReader(r => new Program(p, r).start()))
    } yield ExitCode.Success
}