import scala.collection.mutable.ArrayBuffer

class MDP( start: Int, states: Array[State] ) {
  var willContinue = false

  val bVals: ArrayBuffer[Float] = new ArrayBuffer[Float](states.length)
  (1 to states.length).foreach(_ => bVals += 0)
  val bestActs: ArrayBuffer[Int] = new ArrayBuffer[Int](states.length)
  (1 to states.length).foreach(_ => bestActs += -1)

  override def toString: String = {
    bestActs.map(a => if (a == -1) "" else a.toString).mkString("\n")
  }

  def backup(gamma: Float, tc: Float): Unit = {
    def Utility( s: State ): Unit = {
      MDP.backups += 1

      def utilSum(a: Action): Float = {
        val sucStates = a.sucStates.map(suc => suc._2 * bVals(suc._1))
        sucStates.sum
      }

      val acts = s.actions.map(utilSum)
      var max: Float = 0
      if (!s.isTerminal) max = acts.max
      val actIndex = acts.indexOf(max)
      val stateIndex = states.indexOf(s)
      bestActs(stateIndex) = actIndex
      val newVal = s.reward + (gamma * max)
      if (bVals(stateIndex) - newVal > tc) willContinue = true
      bVals(stateIndex) = newVal
    }

    willContinue = false
    MDP.backups += 1
    this.states.foreach(Utility)
  }


}
object MDP {
  var backups = 0
}

case class State(reward: Float, isTerminal: Boolean, actions: Array[Action]) {
  override def toString: String = {
    val numActions = actions.length
    s"$reward $isTerminal $numActions\n" +
    actions.mkString("")
  }
}

case class Action( sucStates: Array[(Int, Float)] ) {
  override def toString: String = {
    val numSuc = sucStates.length
    sucStates.mkString(s"$numSuc ", " ", "\n")
  }
}