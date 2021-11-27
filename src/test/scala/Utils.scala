import cats.Applicative
import cats.data.Writer
import cats.effect.kernel.Sync

import java.util
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import cats.syntax.all._
object Utils extends App {
  def readerMock[F[_]: Sync](s: String): Reader[F] = {
    val lines = s.split("\n")
    val linesArrayList: ListBuffer[String] = ListBuffer.from(lines)

    new Reader[F] {
      override def readLine: F[String] = Sync[F].delay(linesArrayList.remove(0))
        .handleError(_ => "")
    }
  }
  def printerMock[F[_]: Sync](lb: ListBuffer[String]): Printer[F] = {
    (s: String) => Sync[F].delay(s.split("\n").foreach(s => lb.addOne(s)))
  }
}
