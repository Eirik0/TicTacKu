package tictac

import Rules._
import PlayerMoves._
import Draw.Win
import Draw.WinKu
import Draw.WinCon
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

object Game {
  /**
   * There are two kinds of players: Human and Computer.
   * The computer player is defined by a function that inputs a position and returns a move.
   */
  sealed abstract class Player
  case object Human extends Player
  case class Computer(getMove: (Position => (Int, Int))) extends Player

  /**
   * This class defines the common elements between the two variations of the game.
   */
  sealed abstract class TicTac {
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

    private var _p1Wins = HashSet[Int]()
    def p1Wins_=(p1Wins: HashSet[Int]): Unit = _p1Wins = p1Wins
    def p1Wins = _p1Wins

    private var _p2Wins = HashSet[Int]()
    def p2Wins_=(p2Wins: HashSet[Int]): Unit = _p2Wins = p2Wins
    def p2Wins = _p2Wins

    private var _draws = HashSet[Int]()
    def draws_=(draws: HashSet[Int]): Unit = _draws = draws
    def draws = _draws

    /**
     * The methods common to the different variations of the game.
     */
    // New game
    def reset() = {
      board = BoardUtils.emptyBoard
      activeBoard = -1
      isP1First = !isP1First
      isP1Turn = isP1First
      p1Wins = HashSet[Int]()
      p2Wins = HashSet[Int]()
      draws = HashSet[Int]()
    }

    def makeMove(b: Int, sb: Int) = {
      if (board(b)(sb) != ' ') throw new IllegalStateException(board(b)(sb).toString)

      board(b)(sb) = if (isP1Turn) 'X' else 'O'
      activeBoard = getUpdatedActive(sb)

      // Check for a new win
      val maybeWin = {
        if (!p1Wins.contains(b) && !p2Wins.contains(b) && !draws.contains(b)) {
          BoardUtils.findWin(board(b)) match {
            case Some((w1, w2)) => {
              val (x0, y0) = BoardUtils.getBoardXY(b, w1)
              val (x1, y1) = BoardUtils.getBoardXY(b, w2)
              if (isP1Turn) p1Wins += b else p2Wins += b
              Some(getWin(b, x0, y0, x1, y1, isP1Turn))
            }
            case None => {
              if (board(b).forall(_ != ' ')) draws += b
              None
            }
          }
        } else {
          None
        }
      }

      moves += 1
      isP1Turn = !isP1Turn

      maybeWin
    }

    // Returns the position for the computer to analyze
    def getPosition = Position(board, activeBoard, isP1Turn, p1Wins, p2Wins, draws, allLegalMoves, getUpdatedActive)

    // Returns the move which the computer would make in the current position
    private def getComputerMove = {
      val currentPlayer = if (isP1Turn) player1 else player2
      currentPlayer match {
        case computerPlayer @ Computer(getMove) => computerPlayer.getMove(getPosition)
        case player => throw new IllegalStateException(player.toString)
      }
    }

    // Makes the move that the computer would make in the current position
    def makeComputerMove(): Option[Win] = {
      if (!isComputerThinking) {
        isComputerThinking = true
        try {
          val (b, sb) = getComputerMove
          makeMove(b, sb)
        } finally {
          isComputerThinking = false
        }
      } else {
        None
      }
    }

    def isValidMove(b: Int, sb: Int) = {
      if (b < 0 || b > 8 || sb < 0 || sb > 8) false
      else if (board(b)(sb) == ' ') true
      else false
    }

    val allLegalMoves = (p: Position) => {
      if (p.p1Wins.size != 5 && p.p2Wins.size != 5 && p.p1Wins.size + p.p2Wins.size + p.draws.size != 9) {
        if (p.activeBoard == -1) {
          wholeBoardMoves(p)
        } else {
          BoardUtils.getMovesInSubboard(p.board, p.activeBoard)
        }
      } else {
        ArrayBuffer[(Int, Int)]()
      }
    }

    /**
     * Game-Specific Methods
     */
    val title: String
    def isGameOver: Boolean
    def doesP1Win: Boolean
    def doesP2Win: Boolean

    protected val wholeBoardMoves: (Position => ArrayBuffer[(Int, Int)])
    protected val getUpdatedActive: (Int => Int)
    protected def getWin(b: Int, x0: Int, y0: Int, x1: Int, y1: Int, isP1Turn: Boolean): Win
  }

  /**
   * Tic Tac Ku
   */
  object TicTacKu extends TicTac {
    val title = "Tic Tac Ku"

    def isGameOver = p1Wins.size == 5 || p2Wins.size == 5 || p1Wins.size + p2Wins.size + draws.size == 9
    def doesP1Win = p1Wins.size > p2Wins.size
    def doesP2Win = p1Wins.size < p2Wins.size

    val wholeBoardMoves = (p: Position) => {
      var moves = ArrayBuffer[(Int, Int)]()
      var b = 0

      do {
        moves ++= BoardUtils.getMovesInSubboard(p.board, b)
        b += 1
      } while (b < p.board.size)

      moves
    }

    val getUpdatedActive = (sb: Int) => {
      val isSbFull = board(sb).forall(_ != ' ')
      if (isSbFull) -1 else sb
    }

    def getWin(b: Int, x0: Int, y0: Int, x1: Int, y1: Int, isP1Turn: Boolean) = WinKu(x0, y0, x1, y1, isP1Turn)
  }

  /**
   * Tic Tac Con
   */
  object TicTacCon extends TicTac {
    val title = "Tic Tac Con"

    def isGameOver = doesP1Win || doesP2Win || p1Wins.size + p2Wins.size + draws.size == 9
    def doesP1Win = BoardUtils.findWin(getWinBoard(p1Wins)) != None
    def doesP2Win = BoardUtils.findWin(getWinBoard(p2Wins)) != None

    val wholeBoardMoves = (p: Position) => {
      val ownedBoards = p1Wins ++ p2Wins ++ draws
      var moves = ArrayBuffer[(Int, Int)]()
      var b = 0

      do {
        if (!ownedBoards.contains(b)) moves ++= BoardUtils.getMovesInSubboard(p.board, b)
        b += 1
      } while (b < p.board.size)

      moves
    }

    val getUpdatedActive = (sb: Int) => {
      val ownedBoards = p1Wins ++ p2Wins ++ draws
      if (ownedBoards.contains(sb) || board(sb).forall(_ != ' ')) -1 else sb
    }

    def getWin(b: Int, x0: Int, y0: Int, x1: Int, y1: Int, isP1Turn: Boolean) = WinCon(b, isP1Turn)

    def getWinBoard(wins: HashSet[Int]) = {
      var winBoard = ArrayBuffer(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')
      for (x <- wins) winBoard(x) = 'X'
      winBoard
    }
  }

  /**
   * Position and Rules
   */
  case class Position(board: ArrayBuffer[ArrayBuffer[Char]], activeBoard: Int, isP1Turn: Boolean,
    p1Wins: HashSet[Int], p2Wins: HashSet[Int], draws: HashSet[Int], allLegalMoves: (Position => ArrayBuffer[(Int, Int)]),
    updateActive: (Int => Int)) {
    def legalMoves = allLegalMoves(this)
  }

  // Position, above, is scored in integers, players are booleans and moves are pairs of integers.
  object TicTacRules extends GameRules[Position, Int, Boolean, (Int, Int)] {
    def children(position: Position): List[Position] = position.legalMoves.par.map(update(_)(position)).toList

    def update(move: (Int, Int))(position: Position): Position = {
      val (b, sb) = move
      var newBoard = ArrayBuffer[ArrayBuffer[Char]]()
      position.board.foreach(newBoard += _.clone)

      var newP1Wins = position.p1Wins.clone
      var newP2Wins = position.p2Wins.clone
      var newDraws = position.draws.clone

      var subboard = newBoard(b)
      val isFull = subboard.forall(_ != ' ')

      val xo = if (position.isP1Turn) 'X' else 'O'
      subboard(sb) = xo

      if (!position.p1Wins.contains(b) && !position.p2Wins.contains(b) && !position.draws.contains(b)) {
        BoardUtils.findWin(subboard) match {
          case Some(_) => {
            if (position.isP1Turn) newP1Wins += b else newP2Wins += b
          }
          case None => if (isFull) newDraws += b
        }
      }

      Position(newBoard, position.updateActive(sb), !position.isP1Turn, newP1Wins, newP2Wins, newDraws, position.allLegalMoves, position.updateActive)
    }

    def turn(position: Position) = position.isP1Turn
  }
}