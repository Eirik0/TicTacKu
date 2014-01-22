package tictac

import Game._
import Boards._
import Strategy._
import Oracles._
import jplayer._

object PlayerMoves {
  // Random
  def getRandomMove(position: Position) = {
    def rand = (new scala.util.Random).nextInt(9)

    var an = position.activeBoard
    var guess = rand

    if (an == -1) {
      an = rand
      while (position.board(an).forall(_ != ' ')) an = rand

      var activeBoard = position.board(an)
      while (activeBoard(guess) != ' ') guess = rand

    } else {
      var activeBoard = position.board(position.activeBoard)
      while (activeBoard(guess) != ' ') guess = rand
    }

    (an, guess)
  }

  // Minimax
  def getMoveMinimax(oracle: GameOracle[Position, Int, Boolean], depth: Int, position: Position): (Int, Int) = {
    val strategy = MinimaxStrategy[Position, Int, Boolean](depth)

    val movesList = NonEmptyListUtils.fromList(position.legalMoves.toList) match {
      case Some(move) => move
      case None => {
        println(position.board.mkString("", "\n", "\n") + position.isP1Turn + " " + position.activeBoard)
        println(position.state)
        throw new IllegalStateException("No moves?")
      }
    }

    strategy.pickMove(TicTacRules, oracle, IntOrdering, position, TicTacRules.update(_: (Int, Int))(position), movesList);
  }

  def getMovej(position: Position) = {
    val board = position.board
    val activeBoard = position.activeBoard
    val isP1Turn = position.isP1Turn
    val inactiveBoards = position.state.inactive

    val boardj = new java.util.ArrayList[java.util.ArrayList[Square]]
    for (i <- 0 to 8) {
      val subboardj = new java.util.ArrayList[Square]
      for (j <- 0 to 8) {
        subboardj.add(new Square(board(i)(j)))
      }
      boardj.add(subboardj)
    }

    val p1Winsj = new java.util.ArrayList[Cell]
    val p2Winsj = new java.util.ArrayList[Cell]
    val drawsj = new java.util.ArrayList[Cell]

    for (i <- 0 to 8) {
      val player = getPlayerFromBoard(inactiveBoards, i)
      if (player == Boards.p1) p1Winsj.add(new Cell(i))
      else if (player == Boards.p2) p2Winsj.add(new Cell(i))
      else if (player == Boards.draw) drawsj.add(new Cell(i))
    }

    val player = new JPlayer

    val move = player.getMoveExample(boardj, activeBoard, isP1Turn, p1Winsj, p2Winsj, drawsj)

    if (board(move.activeBoard)(move.square) != ' ') throw new IllegalStateException(move.activeBoard + " " + move.square)

    (move.activeBoard, move.square)
  }
}