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

  object BothPossibleWins extends GameOracle[Position, Int, Boolean] {
    def score(player: Boolean, game: Position): Int = {
      val me = if (player) 'X' else 'O'
      val them = if (player) 'O' else 'X'
      val myBoards = if (player) game.p1Wins else game.p2Wins
      val theirBoards = if (player) game.p2Wins else game.p1Wins
      var myScore = 0

      var b = 0
      do {
        if (!game.ownedBoards.contains(b)) {
          val board = game.board(b)
          var myWins = 8
          var theirWins = 8

          if (!board.forall(_ == ' ')) {
            val b4 = board(4)

            if (b4 == them) {
              myWins -= 4
            } else if (b4 == me) {
              theirWins -= 4
            } else {
              if (board(3) == them || board(5) == them) myWins -= 1

              if (board(1) == them || board(7) == them) myWins -= 1

              if (board(0) == them || board(8) == them) myWins -= 1
              if (board(2) == them || board(6) == them) myWins -= 1

              if (board(3) == me || board(5) == me) theirWins -= 1

              if (board(1) == me || board(7) == me) theirWins -= 1

              if (board(0) == me || board(8) == me) theirWins -= 1
              if (board(2) == me || board(6) == me) theirWins -= 1
            }
            if (board(0) == them || board(1) == them || board(2) == them) myWins -= 1
            if (board(6) == them || board(7) == them || board(8) == them) myWins -= 1

            if (board(0) == them || board(3) == them || board(6) == them) myWins -= 1
            if (board(2) == them || board(5) == them || board(8) == them) myWins -= 1

            if (board(0) == me || board(1) == me || board(2) == me) theirWins -= 1
            if (board(6) == me || board(7) == me || board(8) == me) theirWins -= 1

            if (board(0) == me || board(3) == me || board(6) == me) theirWins -= 1
            if (board(2) == me || board(5) == me || board(8) == me) theirWins -= 1
          }
          myScore += myWins - theirWins + BoardUtils.count2s(board, player) - BoardUtils.count2s(board, !player)
        }

        b += 1
      } while (b < 9)

      myScore + 8 * (myBoards - theirBoards)
    }
  }
}