package tictac

object Rules {
  /**
   * By Nick Vanderweit
   */

  /** Game rules for state space S, scores R, players in P, and moves in M */
  trait GameRules[S, R, P, M] {
    /**
     * Score a position relative to a player. That is, a better
     * position should have a bigger score than a worse one.
     */
    def children(position: S): List[S]
    /** Try to apply a move to some position */
    def update(move: M)(position: S): S
    def turn(position: S): P // Whose turn is it on some position?
  }

  /**
   * Scores positions for a Strategy. R should be ordered (see the signature of
   * pickMove below) and should be positive if the given player has an advantage.
   */
  trait GameOracle[S, R, P] {
    def score(player: P, position: S): R
  }

  /** Strategy over a state space S with scores in R and players in P */
  trait Strategy[S, R, P] {
    /**
     * Universally quantified in M, so that the only possible return
     * values are elements of the candidates list
     */
    def pickMove[M](rules: GameRules[S, R, P, M], oracle: GameOracle[S, R, P],
      ord: Ordering[R], position: S, candidates: List[M]): Option[M]
  }

  // Int Ordering
  object IntOrdering extends Ordering[Int] { def compare(a: Int, b: Int): Int = a - b }

  /**
   * Minimax
   */
  case class MinimaxStrategy[S, R, P](searchDepth: Int) extends Strategy[S, R, P] {
    def pickMove[M](rules: GameRules[S, R, P, M], oracle: GameOracle[S, R, P],
      ord: Ordering[R], position: S, candidates: List[M]): Option[M] = {
      val me = rules.turn(position)

      implicit val ordering = ord
      def scoreByDepth(pos: S, depth: Int): R = {
        if (depth < 1) oracle.score(me, pos)
        else {
          val subscores = rules.children(pos).par.map(scoreByDepth(_, depth - 1))
          // If there are no legal moves from here, return the score
          // as it stands
          if (subscores.isEmpty) oracle.score(me, pos)
          // If it's my turn, I will choose the subtree that maximizes my score
          else if (rules.turn(pos) == me) subscores.maxBy[R](x => x)
          // If it's their turn, they will choose the subtree that minimizes my score
          else subscores.minBy[R](x => x)
        }
      }

      val scoredMoves = candidates.flatMap(move => List((move, scoreByDepth(rules.update(move)(position), searchDepth - 1))))

      if (scoredMoves.isEmpty) None
      else {
        // Pick a random best move
        val max = scoredMoves.maxBy[R]({ case (move, score) => score })._2
        val bestMoves = scoredMoves.filter { case (m, r) => r == max }
        Some(bestMoves((new scala.util.Random).nextInt(bestMoves.size))._1)
      }
    }
  }
}