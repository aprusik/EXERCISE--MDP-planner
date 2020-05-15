object MDPApp {
  def main(args: Array[String]): Unit = {
    if (args.length != 3) {
      println(s"Usage: run.sh vi [discount factor] [termination criterion]")
    } else {
      if (args(0) != "vi") {
        val arg = args(0)
        throw new IllegalArgumentException(
          s"'$arg' not implemented.  Use 'vi' instead."
        )
      }

      val mdp = MDPParser.ParseStdIn
      do {
        mdp.backup(args(1).toFloat, args(2).toFloat)
      } while (mdp.willContinue)
      println(mdp)
      val backups = MDP.backups
      println(s"$backups backups performed.")
    }
  }
}