package tictac

import Strategy._
import Game._
import scala.collection.mutable.ArrayBuffer

object Oracles {
  abstract trait TicTacOracle extends GameOracle[Position, Int, Char] {
    val victory = 1000
    val requiresAdditionalScoring = false

    def winBonus(position: Position, player: Char) = victory * {
      if (player == 'X') {
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

    // 'X' = 1011000
    // 'O' = 1001111
    //  ^  = 0010111
    // 'X' ^ 23 = 'O', 'O' ^ 23 = 'X'
    def scoreByActiveBoard(player: Char, board: ArrayBuffer[Char]): Int

    def additionalScoring(player: Char, position: Position): Int

    def score(player: Char, position: Position): Int = {
      var score = wonBoard * (if (player == 'X') (position.state.p1Wins - position.state.p2Wins) else (position.state.p2Wins - position.state.p1Wins))
      val inactive = position.state.inactive
      // The following is equivalent to looping and calling isBoardInPlay 
      if (((inactive >> 0) & 3) == 0) scoreByActiveBoard(player, position.board(0))
      if (((inactive >> 2) & 3) == 0) scoreByActiveBoard(player, position.board(1))
      if (((inactive >> 4) & 3) == 0) scoreByActiveBoard(player, position.board(2))
      if (((inactive >> 6) & 3) == 0) scoreByActiveBoard(player, position.board(3))
      if (((inactive >> 8) & 3) == 0) scoreByActiveBoard(player, position.board(4))
      if (((inactive >> 10) & 3) == 0) scoreByActiveBoard(player, position.board(5))
      if (((inactive >> 12) & 3) == 0) scoreByActiveBoard(player, position.board(6))
      if (((inactive >> 14) & 3) == 0) scoreByActiveBoard(player, position.board(7))
      if (((inactive >> 16) & 3) == 0) scoreByActiveBoard(player, position.board(8))

      if (requiresAdditionalScoring) score + additionalScoring(player, position) else score
    }
  }

  /**
   * Scores by trying to win out right, then by trying to win eventually, and finally by counting open ended twos.
   */
  object FastWin extends TicTacOracle {
    val twoScore = 5
    val wonBoard = 100
    val fastVictoryScore = 10000
    override val requiresAdditionalScoring = true

    def scoreByActiveBoard(player: Char, board: ArrayBuffer[Char]) = BoardUtils.count2s(board, player) - BoardUtils.count2s(board, player ^ 23)

    def additionalScoring(player: Char, position: Position) = {
      var additionalScore = 0

      if (position.isGameOver) {
        val (p1TotalVictory, p2TotalVictory) = (position.state.p1Wins == 5, position.state.p2Wins == 5)
        if (p2TotalVictory || p2TotalVictory) {
          var totalMoves = 0
          position.board.foreach(sb => sb.foreach(c => if (c != ' ') totalMoves += 10))
          if (player == 'X') fastVictoryScore - totalMoves else -(fastVictoryScore - totalMoves)
        }
      }

      additionalScore
    }
  }

  /**
   * Tries to win, then tries goes for possible wins, and then counts open ended twos.
   */
  object PossibleWins extends TicTacOracle {
    val wonBoard = 8

    def scoreByActiveBoard(player: Char, board: ArrayBuffer[Char]) = {
      BoardUtils.scorePossibleWins(board, player) + (BoardUtils.count2s(board, player) - BoardUtils.count2s(board, player ^ 23))
    }

    def additionalScoring(player: Char, position: Position) = 0
  }

  /**
   * Tries to win, then goes for moves that are one away from a win, and then two away.
   */
  object MovesToWin extends TicTacOracle {
    val wonBoard = 56

    def scoreByActiveBoard(player: Char, board: ArrayBuffer[Char]) = {
      7 * BoardUtils.scoreWinsInOne(board, player) + BoardUtils.scoreWinsInTwo(board, player)
    }

    def additionalScoring(player: Char, position: Position) = 0
  }
}