package tictac

import Strategy._
import Game._

object Oracles {
  abstract trait TicTacOracle extends GameOracle[Position, Int, Boolean] {
    val win = 100
    def winBonus(position: Position, player: Boolean) = win * {
      if (player) {
        if (position.doesP1Win) 1
        else if (position.doesP2Win) -1
        else 0
      } else {
        if (position.doesP2Win) 1
        else if (position.doesP1Win) -1
        else 0
      }
    }
  }

  object FastWin extends TicTacOracle {
    val two = 5
    val victory = 1000
    val fastVictory = 10000
    def score(player: Boolean, position: Position): Int = {
      var score = 0

      val p12s = (0 to 8).toList.filter(!position.ownedBoards.contains(_)).par.map(b => BoardUtils.count2s(position.board(b), true)).sum
      val p22s = (0 to 8).toList.filter(!position.ownedBoards.contains(_)).par.map(b => BoardUtils.count2s(position.board(b), false)).sum

      var totalMoves = 0
      position.board.foreach(sb => sb.foreach(c => if (c != ' ') totalMoves += 10))

      if (player) {
        if (position.p1Wins.size == 5) score += fastVictory - totalMoves
        else if (position.doesP1Win) score += victory
        else {
          score += win * (position.p1Wins.size - position.p2Wins.size)
          score += two * (p12s - p22s)
        }
      } else {
        if (position.p2Wins == 5) score += fastVictory - totalMoves
        else if (position.doesP2Win) score += victory
        else {
          score += win * (position.p2Wins.size - position.p1Wins.size)
          score += two * (p22s - p12s)
        }
      }

      score
    }
  }

  object PossibleWins extends TicTacOracle {
    def score(player: Boolean, position: Position): Int = {
      val me = if (player) 'X' else 'O'
      val them = if (player) 'O' else 'X'
      val myBoards = if (player) position.p1Wins.size else position.p2Wins.size
      val theirBoards = if (player) position.p2Wins.size else position.p1Wins.size

      var myScore = 0

      val ownedBoards = position.p1Wins ++ position.p2Wins ++ position.draws
      var b = 0
      do {
        if (!ownedBoards.contains(b)) myScore += BoardUtils.scorePossibleWins(position.board(b), me, them)
        b += 1
      } while (b < 9)

      myScore + 8 * (myBoards - theirBoards) + winBonus(position, player)
    }
  }

  object MovesToWin extends TicTacOracle {
    def score(player: Boolean, position: Position): Int = {
      val me = if (player) 'X' else 'O'
      val them = if (player) 'O' else 'X'
      val myBoards = if (player) position.p1Wins.size else position.p2Wins.size
      val theirBoards = if (player) position.p2Wins.size else position.p1Wins.size

      var myOneAways = 0
      var theirOneAways = 0

      val ownedBoards = position.p1Wins ++ position.p2Wins ++ position.draws
      var b = 0
      do {
        if (!ownedBoards.contains(b)) {
          val currentBoard = position.board(b)
          for (i <- 0 to 8) {
            if (currentBoard(i) == ' ') {
              var clone = currentBoard.clone
              clone(i) = me
              if (BoardUtils.findWin(clone, me) != None) myOneAways += 1
              clone(i) = them
              if (BoardUtils.findWin(clone, them) != None) theirOneAways += 1
            }
          }
        }
        b += 1
      } while (b < 9)

      myOneAways - theirOneAways + 8 * (myBoards - theirBoards) + winBonus(position, player)
    }
  }
}