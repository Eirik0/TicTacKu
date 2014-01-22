package tictac

import scala.annotation.switch
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import Boards._

object BoardUtils {
  /**
   * Methods for translating the board to the Gui
   */
  val boardNM = ArrayBuffer(
    (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2),
    (0, 3), (0, 4), (0, 5), (1, 3), (1, 4), (1, 5), (2, 3), (2, 4), (2, 5),
    (0, 6), (0, 7), (0, 8), (1, 6), (1, 7), (1, 8), (2, 6), (2, 7), (2, 8),
    (3, 0), (3, 1), (3, 2), (4, 0), (4, 1), (4, 2), (5, 0), (5, 1), (5, 2),
    (3, 3), (3, 4), (3, 5), (4, 3), (4, 4), (4, 5), (5, 3), (5, 4), (5, 5),
    (3, 6), (3, 7), (3, 8), (4, 6), (4, 7), (4, 8), (5, 6), (5, 7), (5, 8),
    (6, 0), (6, 1), (6, 2), (7, 0), (7, 1), (7, 2), (8, 0), (8, 1), (8, 2),
    (6, 3), (6, 4), (6, 5), (7, 3), (7, 4), (7, 5), (8, 3), (8, 4), (8, 5),
    (6, 6), (6, 7), (6, 8), (7, 6), (7, 7), (7, 8), (8, 6), (8, 7), (8, 8))

  def getBoardNM(x0: Int, y0: Int) = {
    var x = x0
    if (x < 0) x = 0
    if (x > 8) x = 8
    var y = y0
    if (y < 0) y = 0
    if (y > 8) y = 8
    (y * 9 + x) match {
      case n if n >= 0 && n <= 80 => boardNM(n)
      case n => throw new IllegalStateException(n.toString)
    }
  }

  def getBoardXY(b: Int, sb: Int) = {
    val (y, x) = getBoardNM(sb, b)
    (x, y)
  }

  def getActiveBoardXY(n: Int) = {
    (n: @switch) match {
      case 0 => (0, 0)
      case 1 => (1, 0)
      case 2 => (2, 0)
      case 3 => (0, 1)
      case 4 => (1, 1)
      case 5 => (2, 1)
      case 6 => (0, 2)
      case 7 => (1, 2)
      case 8 => (2, 2)
      case n => throw new IllegalStateException(n.toString)
    }
  }

  /**
   * Methods that assist with game flow mechanics.
   */
  def getMovesInSubboard(board: ArrayBuffer[ArrayBuffer[Char]], activeBoard: Int) = {
    val subboard = board(activeBoard)
    var moves = ArrayBuffer[(Int, Int)]()
    var square = 0
    do {
      if (subboard(square) == ' ') moves += ((activeBoard, square))
      square += 1
    } while (square < 9)
    moves
  }

  def emptyBoard = {
    var board = ArrayBuffer[ArrayBuffer[Char]]()
    val subboard = ArrayBuffer(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')
    for (i <- 1 to 9) board += subboard.clone
    board
  }

  def getWinBoard(wins: BoardState, player: Char) = {
    val me = if (player == 'X') 1 else 2
    var winBoard = ArrayBuffer(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')
    var square = 0
    do {
      if (getPlayerFromBoard(wins.inactive, square) == me) winBoard(square) = player
      square += 1
    } while (square < 9)
    winBoard
  }

  def isFull(b: ArrayBuffer[Char]) = (b(0) != ' ' && b(1) != ' ' && b(2) != ' ' && b(3) != ' ' && b(4) != ' ' && b(5) != ' ' && b(6) != ' ' && b(7) != ' ' && b(8) != ' ')

  /**
   * Finding Wins
   */
  def findWin(sb: ArrayBuffer[Char], player: Char): Option[(Int, Int)] = {
    if (getScoredLine(sb, 0).count(player) == 3) return Some(0, 2)
    if (getScoredLine(sb, 1).count(player) == 3) return Some(3, 5)
    if (getScoredLine(sb, 2).count(player) == 3) return Some(6, 8)
    if (getScoredLine(sb, 3).count(player) == 3) return Some(0, 6)
    if (getScoredLine(sb, 4).count(player) == 3) return Some(1, 7)
    if (getScoredLine(sb, 5).count(player) == 3) return Some(2, 8)
    if (getScoredLine(sb, 6).count(player) == 3) return Some(0, 8)
    if (getScoredLine(sb, 7).count(player) == 3) return Some(2, 6)

    None
  }

  def findWin(board: ArrayBuffer[Char]): Option[(Int, Int)] = {
    val xWins = findWin(board, 'X')
    if (xWins != None) return xWins
    val oWins = findWin(board, 'O')
    if (oWins != None) return oWins
    None
  }

  def winExists(b: ArrayBuffer[Char], player: Char): Boolean = {
    // win with middle
    if (b(4) == player) {
      if (b(0) == player && b(8) == player) return true
      if (b(2) == player && b(6) == player) return true
      if (b(1) == player && b(7) == player) return true
      if (b(3) == player && b(5) == player) return true
    }
    // win with upper left but not middle
    if (b(0) == player) {
      if (b(1) == player && b(2) == player) return true
      if (b(3) == player && b(6) == player) return true
    }
    // win with bottom right but not middle (or upper left)
    if (b(8) == player) {
      if (b(2) == player && b(5) == player) return true
      if (b(6) == player && b(7) == player) return true
    }

    false
  }

  /**
   * Methods that assist with Oracle's scoring.
   */
  // ' ' = 32 = 0100000
  // 'X' = 88 = 1011000
  // 'O' = 79 = 1001111
  case class ScoredLine(line: (Char, Char, Char)) {
    val (x, o, blank): (Int, Int, Int) = ((line._1 << 14) + (line._2 << 7) + line._3: @switch) match {
      case 528416 => (0, 0, 3) //___
      case 528463 => (0, 1, 2) //__O
      case 528472 => (1, 0, 2) //__X
      case 534432 => (0, 1, 2) //_O_
      case 534479 => (0, 2, 1) //_OO
      case 534488 => (1, 1, 1) //_OX
      case 535584 => (1, 0, 2) //_X_
      case 535631 => (1, 1, 1) //_XO
      case 535640 => (2, 0, 1) //_XX
      case 1298464 => (0, 1, 2) //O__
      case 1298511 => (0, 2, 1) //O_O
      case 1298520 => (1, 1, 1) //O_X
      case 1304480 => (0, 2, 1) //OO_
      case 1304527 => (0, 3, 0) //OOO
      case 1304536 => (1, 2, 0) //OOX
      case 1305632 => (1, 1, 1) //OX_
      case 1305679 => (1, 2, 0) //OXO
      case 1305688 => (2, 1, 0) //OXX
      case 1445920 => (1, 0, 2) //X__
      case 1445967 => (1, 1, 1) //X_O
      case 1445976 => (2, 0, 1) //X_X
      case 1451936 => (1, 1, 1) //XO_
      case 1451983 => (1, 2, 0) //XOO
      case 1451992 => (2, 1, 0) //XOX
      case 1453088 => (2, 0, 1) //XX_
      case 1453135 => (2, 1, 0) //XXO
      case 1453144 => (3, 0, 0) //XX...
      case _ => throw new IllegalStateException(line._1 + " " + line._2 + " " + line._3)
    }
    def count(player: Char) = if (player == 'X') x else o
    def countWithBlank(player: Char) = (count(player), blank)
  }

  //   (6)         (7)
  //     \         /
  //      0   1   2 - (0)
  //      3   4   5 - (1)
  //      6   7   8 - (2)
  //      |   |   |
  //     (3) (4) (5)
  def getLine(sb: ArrayBuffer[Char], n: Int) = {
    (n: @switch) match {
      case 0 => (sb(0), sb(1), sb(2))
      case 1 => (sb(3), sb(4), sb(5))
      case 2 => (sb(6), sb(7), sb(8))
      case 3 => (sb(0), sb(3), sb(6))
      case 4 => (sb(1), sb(4), sb(7))
      case 5 => (sb(2), sb(5), sb(8))
      case 6 => (sb(0), sb(4), sb(8))
      case 7 => (sb(2), sb(4), sb(6))
      case n => throw new IllegalStateException(n.toString)
    }
  }

  def getScoredLine(sb: ArrayBuffer[Char], n: Int) = ScoredLine(getLine(sb, n))

  def count2s(b: ArrayBuffer[Char], player: Char) = {
    var twos = 0
    // 2s with square 4
    if (b(4) == player) {
      if (b(0) == player && b(8) == ' ') twos += 1
      else if (b(8) == player && b(0) == ' ') twos += 1
      if (b(1) == player && b(7) == ' ') twos += 1
      else if (b(7) == player && b(1) == ' ') twos += 1
      if (b(2) == player && b(6) == ' ') twos += 1
      else if (b(6) == player && b(2) == ' ') twos += 1
      if (b(3) == player && b(5) == ' ') twos += 1
      else if (b(5) == player && b(3) == ' ') twos += 1
    } else if (b(4) == ' ') {
      if (b(0) == player && b(8) == player) twos += 1
      else if (b(1) == player && b(7) == player) twos += 1
      else if (b(2) == player && b(6) == player) twos += 1
      else if (b(3) == player && b(5) == player) twos += 1
    }
    // 2s with 0 and not 4
    if (b(0) == player) {
      if (b(1) == player && b(2) == ' ') twos += 1
      else if (b(2) == player && b(1) == ' ') twos += 1
      if (b(3) == player && b(6) == ' ') twos += 1
      else if (b(6) == player && b(3) == ' ') twos += 1
    } else if (b(0) == ' ') {
      if (b(1) == player && b(2) == player) twos += 1
      else if (b(3) == player && b(6) == player) twos += 1
    }
    // 2s with 8 and not 0 or 4
    if (b(8) == player) {
      if (b(2) == player && b(5) == ' ') twos += 1
      else if (b(5) == player && b(2) == ' ') twos += 1
      if (b(6) == player && b(7) == ' ') twos += 1
      else if (b(7) == player && b(6) == ' ') twos += 1
    } else if (b(8) == ' ') {
      if (b(2) == player && b(5) == player) twos += 1
      else if (b(6) == player && b(7) == player) twos += 1
    }
    twos
  }

  def scorePossibleWins(b: ArrayBuffer[Char], player: Boolean) = {
    val (me, them) = if (player) ('X', 'O') else ('O', 'X')
    var score = 0
    if (b(3) == them || b(4) == them || b(5) == them) score -= 1
    if (b(6) == them || b(7) == them || b(8) == them) score -= 1
    if (b(0) == them || b(3) == them || b(6) == them) score -= 1
    if (b(1) == them || b(4) == them || b(7) == them) score -= 1
    if (b(2) == them || b(5) == them || b(8) == them) score -= 1
    if (b(0) == them || b(4) == them || b(8) == them) score -= 1
    if (b(2) == them || b(4) == them || b(6) == them) score -= 1

    if (b(3) == me || b(4) == me || b(5) == me) score += 1
    if (b(6) == me || b(7) == me || b(8) == me) score += 1
    if (b(0) == me || b(3) == me || b(6) == me) score += 1
    if (b(1) == me || b(4) == me || b(7) == me) score += 1
    if (b(2) == me || b(5) == me || b(8) == me) score += 1
    if (b(0) == me || b(4) == me || b(8) == me) score += 1
    if (b(2) == me || b(4) == me || b(6) == me) score += 1
    score
  }

  def scoreWinsInOne(currentBoard: ArrayBuffer[Char], player: Boolean) = {
    val (me, them) = if (player) ('X', 'O') else ('O', 'X')
    var myOneAways = 0
    var theirOneAways = 0

    for (i <- 0 to 8) {
      if (currentBoard(i) == ' ') {
        var clone = currentBoard.clone
        clone(i) = me
        if (winExists(clone, me)) myOneAways += 1
        clone(i) = them
        if (winExists(clone, them)) theirOneAways += 1
      }
    }

    myOneAways - theirOneAways
  }

  def scoreWinsInTwo(currentBoard: ArrayBuffer[Char], player: Boolean) = {
    val (me, them) = if (player) ('X', 'O') else ('O', 'X')
    var myTwoAways = 0
    var theirTwoAways = 0

    for (i <- 0 to 8) {
      if (currentBoard(i) == ' ') {
        var clone = currentBoard.clone
        clone(i) = me
        myTwoAways += scoreWinsInOne(clone, player)
        clone(i) = them
        theirTwoAways += scoreWinsInOne(clone, !player)
      }
    }
    myTwoAways - theirTwoAways
  }

}