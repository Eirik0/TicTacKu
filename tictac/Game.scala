package tictac

import Rules._
import PlayerMoves._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object Game {
  /**
   * Players
   */
  sealed abstract class Player

  // Human
  case object Human extends Player

  // Computer
  case class Computer(getMove: (Position => (Int, Int))) extends Player

  /**
   * The game
   */
  object TicTacKu {
    private var _board = BoardUtils.emptyBoard
    def board_=(board: ArrayBuffer[ArrayBuffer[Char]]): Unit = _board = board
    def board = _board

    private var _activeBoard = -1
    def activeBoard_=(activeBoard: Int): Unit = _activeBoard = activeBoard
    def activeBoard = _activeBoard

    private var _player1: Player = Human
    def player1_=(player1: Player): Unit = _player1 = player1
    def player1 = _player1

    private var _player2: Player = Human
    def player2_=(player2: Player): Unit = _player2 = player2
    def player2 = _player2

    private var _isP1First = true
    def isP1First_=(isP1First: Boolean): Unit = _isP1First = isP1First
    def isP1First = _isP1First

    private var _isP1Turn = true
    def isP1Turn_=(isP1Turn: Boolean): Unit = _isP1Turn = isP1Turn
    def isP1Turn = _isP1Turn

    private var _isComputerThinking = false
    def isComputerThinking_=(isComputerThinking: Boolean): Unit = _isComputerThinking = isComputerThinking
    def isComputerThinking = _isComputerThinking

    private var _moves = 0
    def moves_=(moves: Int): Unit = _moves = moves
    def moves = _moves

    private var _wonBoards = HashMap[Int, Win]()
    def wonBoards_=(wonBoards: HashMap[Int, Win]): Unit = _wonBoards = wonBoards
    def wonBoards = _wonBoards

    private var _p1Wins = 0
    def p1Wins_=(p1Wins: Int): Unit = _p1Wins = p1Wins
    def p1Wins = _p1Wins

    private var _p2Wins = 0
    def p2Wins_=(p2Wins: Int): Unit = _p2Wins = p2Wins
    def p2Wins = _p2Wins

    private var _draws = 0
    def draws_=(draws: Int): Unit = _draws = draws
    def draws = _draws

    // Returns the players as a pair
    def players = (player1, player2)

    def isValidMove(b: Int, sb: Int) = {
      if (b < 0 || b > 8 || sb < 0 || sb > 8) false
      else if (board(b)(sb) == ' ') true
      else false
    }

    // Increments the turn 
    private def nextTurn = {
      moves += 1
      isP1Turn = !isP1Turn
    }

    // Make a move
    def makeMove(b: Int, sb: Int) = {
      if (board(b)(sb) == ' ') {
        board(b)(sb) = if (isP1Turn) 'X' else 'O'
        val isSbFull = board(sb).forall(_ != ' ')
        activeBoard = if (isSbFull) -1 else sb

        // Check wins
        if (!wonBoards.contains(b)) {
          BoardUtils.findWin(board(b)) match {
            case Some((w1, w2)) => {
              val (x0, y0) = BoardUtils.getBoardXY(b, w1)
              val (x1, y1) = BoardUtils.getBoardXY(b, w2)

              if (isP1Turn) p1Wins = p1Wins + 1 else p2Wins = p2Wins + 1

              wonBoards += (b -> Win(x0, y0, x1, y1, isP1Turn))
            }
            case None => if (board(b).forall(_ != ' ')) draws = draws + 1
          }
        }

        nextTurn
      } else throw new IllegalStateException(board(b)(sb).toString)
    }

    // The computers move
    private def getComputerMove = {
      val currentPlayer = if (isP1Turn) player1 else player2
      currentPlayer match {
        case computerPlayer @ Computer(getMove) => computerPlayer.getMove(getPosition)
        case player => throw new IllegalStateException(player.toString)
      }
    }

    // Make the computer move
    def makeComputerMove() = {
      if (!isComputerThinking) {
        isComputerThinking = true
        try {
          val (b, sb) = getComputerMove
          makeMove(b, sb)
        } finally {
          isComputerThinking = false
        }
      }
    }

    def getPosition = {
      var ownedBoards = ArrayBuffer[Int]()
      wonBoards.keySet.foreach(ownedBoards += _)
      Position(board, activeBoard, isP1Turn, ownedBoards, p1Wins, p2Wins, draws)
    }

    // Game status
    def gameOver = p1Wins == 5 || p2Wins == 5 || p1Wins + p2Wins + draws == 9
    def isP1Winning = p1Wins > p2Wins
    def isP2Winning = p1Wins < p2Wins

    // New game
    def reset() = {
      board = BoardUtils.emptyBoard
      activeBoard = -1
      isP1First = !isP1First
      isP1Turn = isP1First
      wonBoards = HashMap[Int, Win]()
      p1Wins = 0
      p2Wins = 0
      draws = 0
    }
  }

  case class Position(board: ArrayBuffer[ArrayBuffer[Char]], activeBoard: Int, isP1Turn: Boolean, ownedBoards: ArrayBuffer[Int], p1Wins: Int, p2Wins: Int, draws: Int) {
    def gameOver = p1Wins == 5 || p2Wins == 5 || p1Wins + p2Wins + draws == 9

    def legalMoves = {
      var moves = ArrayBuffer[(Int, Int)]()
      if (!gameOver) {
        if (activeBoard == -1) {
          var b = 0
          do {
            var sb = 0
            val subboard = board(b)
            do {
              if (subboard(sb) == ' ') moves += ((b, sb))
              sb += 1
            } while (sb < subboard.size)
            b += 1
          } while (b < board.size)
        } else {
          var sb = 0
          val subboard = board(activeBoard)
          do {
            if (subboard(sb) == ' ') moves += ((activeBoard, sb))
            sb += 1
          } while (sb < subboard.size)
        }
      }
      moves
    }
  }

  // Position, above, is scored in integers, players are booleans and moves are pairs of integers.
  object TicTacKuRules extends GameRules[Position, Int, Boolean, (Int, Int)] {
    def children(game: Position): List[Position] = game.legalMoves.par.map(update(_)(game)).toList

    def update(move: (Int, Int))(position: Position): Position = {
      val (b, sb) = move
      var newBoard = ArrayBuffer[ArrayBuffer[Char]]()
      position.board.foreach(newBoard += _.clone)

      var newP1Wins = position.p1Wins
      var newP2Wins = position.p2Wins
      var newDraws = position.draws
      var newOwnedBoards = position.ownedBoards.clone

      var subboard = newBoard(b)
      val isFull = subboard.forall(_ != ' ')

      val xo = if (position.isP1Turn) 'X' else 'O'
      subboard(sb) = xo

      if (!position.ownedBoards.contains(b)) {
        BoardUtils.findWin(subboard) match {
          case Some(_) => {
            if (position.isP1Turn) newP1Wins += 1 else newP2Wins += 1
            newOwnedBoards += b
          }
          case None => if (isFull) newDraws += 1
        }
      }

      Position(newBoard, if (isFull) -1 else sb, !position.isP1Turn, newOwnedBoards, newP1Wins, newP2Wins, newDraws)
    }

    def turn(position: Position) = position.isP1Turn
  }

  /**
   * Win
   */
  case class Win(x0: Int, y0: Int, x1: Int, y1: Int, isPlayer1: Boolean)
}