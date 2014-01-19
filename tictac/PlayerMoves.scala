package tictac
import Game._
import Rules._
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

    val move = strategy.pickMove(TicTacRules, oracle, IntOrdering, position, position.legalMoves.toList)
    move match {
      case Some(move) => return move
      case None => {
        println(position.board.mkString("", "\n", "\n") + position.isP1Turn + " " + position.activeBoard)
        println(position.p1Wins.mkString(",") + " " + position.p2Wins.mkString(",") + " " + position.draws.mkString(","))
        throw new IllegalStateException("No moves?")
      }
    }
  }

  def getMovej(position: Position) = {
    val board = position.board
    val activeBoard = position.activeBoard
    val isP1Turn = position.isP1Turn
    val p1Wins = position.p1Wins
    val p2Wins = position.p2Wins
    val draws = position.draws

    val boardj = new java.util.ArrayList[java.util.ArrayList[Square]]
    for (i <- 0 to 8) {
      val subboardj = new java.util.ArrayList[Square]
      for (j <- 0 to 8) {
        subboardj.add(new Square(board(i)(j)))
      }
      boardj.add(subboardj)
    }

    val p1Winsj = new java.util.ArrayList[Board]
    p1Wins.foreach(i => p1Winsj.add(new Board(i)))

    val p2Winsj = new java.util.ArrayList[Board]
    p2Wins.foreach(i => p2Winsj.add(new Board(i)))

    val drawsj = new java.util.ArrayList[Board]
    draws.foreach(i => drawsj.add(new Board(i)))

    val player = new JPlayer

    val move = player.getMoveExample(boardj, activeBoard, isP1Turn, p1Winsj, p2Winsj, drawsj)

    if (board(move.activeBoard)(move.square) != ' ') throw new IllegalStateException(move.activeBoard + " " + move.square)

    (move.activeBoard, move.square)
  }
}