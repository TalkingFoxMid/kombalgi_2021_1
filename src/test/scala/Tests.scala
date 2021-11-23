import Main.Program
import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import cats.effect.unsafe.implicits.global

import scala.collection.mutable.ListBuffer
class Test extends AnyFlatSpec {
  def checkResult(input: String, correct: String) = {
    val buffer = ListBuffer.empty[String]
    val printer = Utils.printerMock[IO](buffer)
    val reader = Utils.readerMock[IO](input)
    val program = new Program(printer, reader)
    program.start().unsafeRunSync()
    assert(buffer.mkString == correct)
  }
}
