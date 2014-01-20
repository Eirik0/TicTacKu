package tictac

import Strategy._
import scala.util.Random

case class MinimaxStrategy[S, R, P](searchDepth: Int) extends Strategy[S, R, P] {
    def pickMove[M](rules: GameRules[S, P], oracle: GameOracle[S, R, P],
                    ord: Ordering[R], position: S, translate: M => S,
                    candidates: NonEmptyList[M]): M = {
        val me = rules.turn(position)
        implicit val ordering = ord
        
        def scoreByDepth(pos: S, depth: Int): R =
            if(depth < 1) oracle.score(me, pos)
            else {
                val subscores = rules.children(pos).par.map(scoreByDepth(_, depth - 1))
                // If there are no legal moves from here, return the score
                // as it stands
                if(subscores.isEmpty) oracle.score(me, pos)
                // If it's my turn, I will choose the subtree that maximizes my score
                else if(rules.turn(pos) == me) subscores.maxBy[R](x => x)
                // If it's their turn, they will choose the subtree that minimizes my score
                else subscores.minBy[R](x => x)
            }
        
        val scoredMoves = candidates.map(move => (move, scoreByDepth(translate(move), searchDepth - 1)))
        
        val (maxMove, maxScore) = scoredMoves.maxBy[R]({case (move, score) => score})
        val moves = scoredMoves.fold[NonEmptyList[M]]({
          case (move, score) =>
            if(score == maxScore) Cons(move, Singleton(maxMove))
            else Singleton(maxMove)
        },
        {
          case ((move, score), moves) =>
            if(score == maxScore) Cons(move, moves)
            else moves
        });

        val ms = NonEmptyListUtils.toList(moves)
        ms((new Random()).nextInt(ms.length))
    }
}