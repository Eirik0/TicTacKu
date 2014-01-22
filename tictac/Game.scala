package tictac

import Strategy._
import PlayerMoves._
import Draw.Win
import Draw.WinKu
import Draw.WinCon
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import tictac.Boards._

object Game {
  /**
   * There are two kinds of players: Human and Computer.
   * The computer player is defined by a function that inputs a position and returns a move.
   */
  sealed abstract class Player
  case object Human extends Player
  case class Computer(getMove: (Position => (Int, Int))) extends Player

  /**
   * This trait defines the common elements between the two variations of the game.
   */
  sealed abstract trait TicTac {
    private var _board = BoardUtils.emptyBoard
    def board_=(board: ArrayBuffer[ArrayBuffer[Char]]): Unit = _board = board
    def board = _board

    protected val flow: TicTacFlow

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

    private var _boardState = BoardState(0, 0, 0, 0)
    def boardState_=(boardState: BoardState): Unit = _boardState = boardState
    def boardState = _boardState

    /**
     * The methods common to the different variations of the game.
     */
    // New game
    def reset() = {
      board = BoardUtils.emptyBoard
      activeBoard = -1
      isP1First = !isP1First
      isP1Turn = isP1First
      boardState = BoardState(0, 0, 0, 0)
    }

    def makeMove(b: Int, sb: Int) = {
      if (board(b)(sb) != ' ') throw new IllegalStateException(board(b)(sb).toString)

      board(b)(sb) = if (isP1Turn) 'X' else 'O'

      // Check for a new win
      val maybeWin = {
        if (boardState.isBoardInPlay(b)) BoardUtils.findWin(board(b)) match {
          case Some((w1, w2)) => {
            boardState = boardState.updatedWith(b, if (isP1Turn) Boards.p1 else Boards.p2)
            val (x0, y0) = BoardUtils.getBoardXY(b, w1)
            val (x1, y1) = BoardUtils.getBoardXY(b, w2)
            Some(getWin(b, x0, y0, x1, y1, isP1Turn))
          }
          case None => {
            if (BoardUtils.isFull(board(b))) boardState = boardState.updatedWith(b, Boards.draw)
            None
          }
        }
        else None
      }

      activeBoard = flow.getUpdatedActive(getPosition, sb)
      moves += 1
      isP1Turn = !isP1Turn

      maybeWin
    }

    // Returns the position for the computer to analyze
    def getPosition = Position(board, activeBoard, isP1Turn, boardState, flow)

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

    def isValidMove(b: Int, sb: Int) = flow.isValidMove(getPosition, b, sb)
    def isLegalMove(b: Int, sb: Int) = flow.isLegalMove(getPosition, b, sb)

    def isGameOver = flow.isGameOver(getPosition)
    def doesP1Win = flow.doesP1Win(getPosition)
    def doesP2Win = flow.doesP2Win(getPosition)

    val title: String

    protected def getWin(b: Int, x0: Int, y0: Int, x1: Int, y1: Int, isP1Turn: Boolean): Win
  }

  /**
   * This trait defines the differences between the way positions update
   */
  sealed abstract trait TicTacFlow {
    val wholeBoardMoves: (Position => ArrayBuffer[(Int, Int)])
    val getUpdatedActive: ((Position, Int) => Int)

    val allLegalMoves = (p: Position) => {
      if (!p.isGameOver) {
        if (p.activeBoard == -1) wholeBoardMoves(p)
        else BoardUtils.getMovesInSubboard(p.board, p.activeBoard)
      } else ArrayBuffer[(Int, Int)]()
    }

    val isValidMove = (p: Position, b: Int, sb: Int) => {
      if (b < 0 || b > 8 || sb < 0 || sb > 8) false
      else if (p.board(b)(sb) == ' ') true
      else false
    }

    val isLegalMove: (Position, Int, Int) => Boolean

    val isGameOver: Position => Boolean
    val doesP1Win: Position => Boolean
    val doesP2Win: Position => Boolean
  }

  /**
   * Tic Tac Ku
   */
  object TicTacKu extends TicTac {
    val title = "Tic Tac Ku"
    val flow = FlowKu

    def getWin(b: Int, x0: Int, y0: Int, x1: Int, y1: Int, isP1Turn: Boolean) = WinKu(x0, y0, x1, y1, isP1Turn)
  }

  // Flow for TicTacKu
  object FlowKu extends TicTacFlow {
    val wholeBoardMoves = (p: Position) => ArrayBuffer[(Int, Int)]() ++ (0 to 8).par.flatMap(BoardUtils.getMovesInSubboard(p.board, _))

    val getUpdatedActive = (p: Position, sb: Int) => if (BoardUtils.isFull(p.board(sb))) -1 else sb

    val isLegalMove = (p: Position, b: Int, sb: Int) => isValidMove(p, b, sb)

    val isGameOver = (p: Position) => p.state.p1Wins == 5 || p.state.p2Wins == 5 || p.state.p1Wins + p.state.p2Wins + p.state.draws == 9
    val doesP1Win = (p: Position) => isGameOver(p) && p.state.p1Wins > p.state.p2Wins
    val doesP2Win = (p: Position) => isGameOver(p) && p.state.p1Wins < p.state.p2Wins
  }

  /**
   * Tic Tac Con
   */
  object TicTacCon extends TicTac {
    val title = "Tic Tac Con"
    val flow = FlowCon

    def getWin(b: Int, x0: Int, y0: Int, x1: Int, y1: Int, isP1Turn: Boolean) = WinCon(b, isP1Turn)
  }

  /**
   *  Flow for TicTacCon
   */
  object FlowCon extends TicTacFlow {
    val wholeBoardMoves = (p: Position) => {
      (0 to 8).par.foldLeft(ArrayBuffer[(Int, Int)]()) { (moves, b) =>
        if (p.state.isBoardInPlay(b)) moves ++ BoardUtils.getMovesInSubboard(p.board, b) else moves
      }
    }

    val isLegalMove = (p: Position, b: Int, sb: Int) => isValidMove(p, b, sb) && p.state.isBoardInPlay(b)

    val getUpdatedActive = (p: Position, sb: Int) => if (BoardUtils.isFull(p.board(sb)) || !p.state.isBoardInPlay(sb)) -1 else sb

    val isGameOver = (p: Position) => doesP1Win(p) || doesP2Win(p) || p.state.p1Wins + p.state.p2Wins + p.state.draws == 9
    val doesP1Win = (p: Position) => BoardUtils.winExists((BoardUtils.getWinBoard(p.state, 'X')), 'X')
    val doesP2Win = (p: Position) => BoardUtils.winExists((BoardUtils.getWinBoard(p.state, 'O')), 'O')
  }

  /**
   * Position and Rules
   */
  case class Position(board: ArrayBuffer[ArrayBuffer[Char]], activeBoard: Int, isP1Turn: Boolean, state: BoardState, flow: TicTacFlow) {
    def legalMoves = flow.allLegalMoves(this)
    def updateActive(sb: Int) = flow.getUpdatedActive(this, sb)
    def isGameOver = flow.isGameOver(this)
    def doesP1Win = flow.doesP1Win(this)
    def doesP2Win = flow.doesP2Win(this)
  }

  // Position, above, is scored in integers, players are booleans and moves are pairs of integers.
  object TicTacRules extends GameRules[Position, Boolean] {
    def children(position: Position): List[Position] = position.legalMoves.par.map(update(_)(position)).toList

    def update(move: (Int, Int))(position: Position): Position = {
      val (b, sb) = move
      var newBoard = ArrayBuffer[ArrayBuffer[Char]]()
      position.board.foreach(newBoard += _.clone)

      var newState = position.state

      val xo = if (position.isP1Turn) 'X' else 'O'
      var subboard = newBoard(b)
      subboard(sb) = xo

      if (position.state.isBoardInPlay(b)) {
        if (BoardUtils.winExists(subboard, xo)) {
          newState = newState.updatedWith(b, if (position.isP1Turn) Boards.p1 else Boards.p2)
        } else if (BoardUtils.isFull(subboard)) newState = newState.updatedWith(b, Boards.draw)
      }

      Position(newBoard, position.updateActive(sb), !position.isP1Turn, newState, position.flow)
    }

    def turn(position: Position) = position.isP1Turn
  }
}