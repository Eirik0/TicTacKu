package tictac

import Rules._
import Game._

object Oracles {
  object FastWin extends GameOracle[Position, Int, Boolean] {
    val win = 100
    val two = 5
    val victory = 1000
    val fastVictory = 10000
    def score(player: Boolean, game: Position): Int = {
      var score = 0

      val p1Wins = game.p1Wins
      val p2Wins = game.p2Wins

      val p12s = (0 to 8).toList.filter(!game.ownedBoards.contains(_)).map(b => BoardUtils.count2s(game.board(b), true)).sum
      val p22s = (0 to 8).toList.filter(!game.ownedBoards.contains(_)).map(b => BoardUtils.count2s(game.board(b), false)).sum

      var totalMoves = 0
      game.board.foreach(sb => sb.foreach(c => if (c != ' ') totalMoves += 10))

      if (player) {
        if (game.p1Wins == 5) score += fastVictory - totalMoves
        else if ((game.p1Wins + game.p2Wins + game.draws == 0 && game.p1Wins > game.p2Wins)) score += victory
        else {
          score += win * (p1Wins - p2Wins)
          score += two * (p12s - p22s)
        }
      } else {
        if (game.p2Wins == 5) score += fastVictory - totalMoves
        else if ((game.p1Wins + game.p2Wins + game.draws == 0 && game.p2Wins > game.p1Wins)) score += victory
        else {
          score += win * (p2Wins - p1Wins)
          score += two * (p22s - p12s)
        }
      }

      score
    }
  }
}