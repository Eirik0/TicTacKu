package tictac

import scala.annotation.switch
import scala.collection.mutable.ArrayBuffer

object Boards {
  /* Multiply square by 2 shift player over into that position */
  def addPlayerToBoard(board: Int, square: Int, player: Int) = board | (player << (square << 1))

  /* Multiply square by 2 shift board, mask to get player */
  def getPlayerFromBoard(board: Int, square: Int) = (board >> (square << 1)) & 3

  val empty = 0
  val p1 = 1
  val p2 = 2
  val draw = 3

  case class BoardState(p1Wins: Int, p2Wins: Int, draws: Int, inactive: Int) {
    def isBoardInPlay(b: Int) = ((inactive >> (b << 1)) & 3) == 0
    def updatedWith(square: Int, player: Int) = {
      if (player == p1) BoardState(p1Wins + 1, p2Wins, draws, inactive | (player << (square << 1)))
      else if (player == p2) BoardState(p1Wins, p2Wins + 1, draws, inactive | (player << (square << 1)))
      else if (player == draw) BoardState(p1Wins, p2Wins, draws + 1, inactive | (player << (square << 1)))
      else throw new IllegalStateException(player.toString)
    }
  }
}