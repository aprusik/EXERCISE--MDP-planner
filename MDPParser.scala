import scala.annotation.tailrec
import scala.io.StdIn

object MDPParser {
  @tailrec
  def parseLine: Array[String] = {
    val in = StdIn.readLine()
    if (in.charAt(0) == '#') parseLine
    else in.split(" ")
  }

  def ParseStdIn: MDP = {
    val numStates = parseLine(3).toInt
    val start = parseLine(2).toInt
    val states = for (_ <- 0 until numStates) yield {

      val line = parseLine
      val reward = line(0).toFloat
      val isTerminal = line(1).toInt == 1
      val numActs = line(2).toInt
      val actions = for (_ <- 0 until numActs) yield {

        val line = parseLine
        val numSuc = line(0).toInt
        val it = line.tail.toIterator
        val successors = for (_ <- 0 until numSuc) yield
          (it.next().toInt, it.next().toFloat)
        Action(successors.toArray)

      }
      State(reward, isTerminal, actions.toArray)

    }
    new MDP(start, states.toArray)
  }
}