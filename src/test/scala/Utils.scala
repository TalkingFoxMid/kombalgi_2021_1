import cats.effect.kernel.Sync
import cats.syntax.all._

import scala.collection.mutable.ListBuffer

object Utils extends App {
  def readerMock[F[_] : Sync](s: String): Reader[F] = {
    val lines = s.split("\n")
    val linesArrayList: ListBuffer[String] = ListBuffer.from(lines)

    new Reader[F] {
      override def readLine: F[String] = Sync[F].delay(linesArrayList.remove(0))
        .handleError(_ => "")
    }
  }

  def printerMock[F[_] : Sync](lb: ListBuffer[String]): Printer[F] = new Printer[F] {
    override def printLine(s: String): F[Unit] = Sync[F].delay(lb.addOne(s))

    override def printStr(s: String): F[Unit] = Sync[F].delay(lb.addOne(lb.remove(0) + s))
  }
}
