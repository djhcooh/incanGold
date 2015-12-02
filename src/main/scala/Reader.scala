package ren.kujoka.common
import scala.io.StdIn.readLine
object Reader {
  def readIntLoop(msg1: String, msg2: String, from: Int, to: Int): Int = {
    try {
      val i = readLine(msg1).toInt
      if (i >= from && i <= to) i else throw new NumberFormatException
    } catch {
      case ex: NumberFormatException =>
        println(msg2)
        readIntLoop(msg1, msg2, from, to)
    }
  }

  def readIntLoop(msg1: String, msg2: String): Int = {
    try {
      readLine(msg1).toInt
    } catch {
      case ex: NumberFormatException =>
        println(msg2)
        readIntLoop(msg1, msg2)
    }
  }

  def yesOrNo(msg: String, yes: String, no: String): String = {
    var rl = ""
    do {
      rl = readLine(msg + "(" + yes + "/" + no +") > ")
      if (rl != yes && rl != no) println("Please enter " + yes + " or " + no)
    } while (rl != yes && rl != no)
      rl
  }
}
