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
  it should "work on 2 vertices" in {
    val input =
      """3 3
        |0 1 0
        |1 0 1
        |0 0 0""".stripMargin

    val output =
      """0 1
        |0 0""".stripMargin
    checkResult(input, output)
  }
}