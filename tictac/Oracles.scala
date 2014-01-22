package tictac

import Strategy._
import Game._
import scala.collection.mutable.ArrayBuffer

object Oracles {
  abstract trait TicTacOracle extends GameOracle[Position, Int, Boolean] {
    val victory = 1000
    def winBonus(position: Position, player: Boolean) = victory * {
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

    val wonBoard: Int
    def scoreByActiveBoard(player: Boolean, board: ArrayBuffer[Char]): Int
    def additionalScoring(player: Boolean, position: Position): Int

    def score(player: Boolean, position: Position): Int = {
      var score = wonBoard * (if (player) (position.state.p1Wins - position.state.p2Wins) else (position.state.p2Wins - position.state.p1Wins))

      var b = 0
      do {
        if (position.state.isBoardInPlay(b)) {
          score += scoreByActiveBoard(player, position.board(b))
        }
        b += 1
      } while (b < 9)

      score + additionalScoring(player, position)
    }
  }

  /**
   * Scores by trying to win out right, then by trying to win eventually, and finally by counting open ended twos.
   */
  object FastWin extends TicTacOracle {
    val twoScore = 5
    val wonBoard = 100
    val fastVictoryScore = 10000

    def scoreByActiveBoard(player: Boolean, board: ArrayBuffer[Char]) = {
      (if (player) BoardUtils.count2s(board, 'X') - BoardUtils.count2s(board, 'O')
      else BoardUtils.count2s(board, 'O') - BoardUtils.count2s(board, 'X')) * twoScore
    }

    def additionalScoring(player: Boolean, position: Position) = {
      var additionalScore = 0

      if (position.isGameOver) {
        val (p1TotalVictory, p2TotalVictory) = (position.state.p1Wins == 5, position.state.p2Wins == 5)
        if (p2TotalVictory || p2TotalVictory) {
          var totalMoves = 0
          position.board.foreach(sb => sb.foreach(c => if (c != ' ') totalMoves += 10))
          // If the person who won is the person whos turn it is
          if (player == p1TotalVictory) fastVictoryScore - totalMoves
          else -(fastVictoryScore - totalMoves)
        }
      }

      additionalScore
    }
  }

  /**
   * Tries to win, then tries goes for possible wins, and then counts open ended twos.
   */
  object PossibleWins extends TicTacOracle {
    val wonBoard = 14

    def scoreByActiveBoard(player: Boolean, board: ArrayBuffer[Char]) = {
      BoardUtils.scorePossibleWins(board, player) + {
        if (player) (BoardUtils.count2s(board, 'X') - BoardUtils.count2s(board, 'O'))
        else (BoardUtils.count2s(board, 'O') - BoardUtils.count2s(board, 'X'))
      }
    }

    def additionalScoring(player: Boolean, position: Position) = 0
  }

  /**
   * Tries to win, then goes for moves that are one away from a win, and then two away.
   */
  object MovesToWin extends TicTacOracle {
    val wonBoard = 112

    def scoreByActiveBoard(player: Boolean, board: ArrayBuffer[Char]) = {
      7 * BoardUtils.scoreWinsInOne(board, player) + BoardUtils.scoreWinsInTwo(board, player)
    }

    def additionalScoring(player: Boolean, position: Position) = 0
  }
}