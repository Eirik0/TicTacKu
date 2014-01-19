package tictac

import scala.annotation.switch
import scala.collection.mutable.ArrayBuffer

object BoardUtils {
  def getBoardNM(x0: Int, y0: Int) = {
    var x = x0
    if (x < 0) x = 0
    if (x > 8) x = 8

    var y = y0
    if (y < 0) y = 0
    if (y > 8) y = 8

    (y * 9 + x: @switch) match {
      case 0 => (0, 0)
      case 1 => (0, 1)
      case 2 => (0, 2)
      case 3 => (1, 0)
      case 4 => (1, 1)
      case 5 => (1, 2)
      case 6 => (2, 0)
      case 7 => (2, 1)
      case 8 => (2, 2)

      case 9 => (0, 3)
      case 10 => (0, 4)
      case 11 => (0, 5)
      case 12 => (1, 3)
      case 13 => (1, 4)
      case 14 => (1, 5)
      case 15 => (2, 3)
      case 16 => (2, 4)
      case 17 => (2, 5)

      case 18 => (0, 6)
      case 19 => (0, 7)
      case 20 => (0, 8)
      case 21 => (1, 6)
      case 22 => (1, 7)
      case 23 => (1, 8)
      case 24 => (2, 6)
      case 25 => (2, 7)
      case 26 => (2, 8)

      case 27 => (3, 0)
      case 28 => (3, 1)
      case 29 => (3, 2)
      case 30 => (4, 0)
      case 31 => (4, 1)
      case 32 => (4, 2)
      case 33 => (5, 0)
      case 34 => (5, 1)
      case 35 => (5, 2)

      case 36 => (3, 3)
      case 37 => (3, 4)
      case 38 => (3, 5)
      case 39 => (4, 3)
      case 40 => (4, 4)
      case 41 => (4, 5)
      case 42 => (5, 3)
      case 43 => (5, 4)
      case 44 => (5, 5)

      case 45 => (3, 6)
      case 46 => (3, 7)
      case 47 => (3, 8)
      case 48 => (4, 6)
      case 49 => (4, 7)
      case 50 => (4, 8)
      case 51 => (5, 6)
      case 52 => (5, 7)
      case 53 => (5, 8)

      case 54 => (6, 0)
      case 55 => (6, 1)
      case 56 => (6, 2)
      case 57 => (7, 0)
      case 58 => (7, 1)
      case 59 => (7, 2)
      case 60 => (8, 0)
      case 61 => (8, 1)
      case 62 => (8, 2)

      case 63 => (6, 3)
      case 64 => (6, 4)
      case 65 => (6, 5)
      case 66 => (7, 3)
      case 67 => (7, 4)
      case 68 => (7, 5)
      case 69 => (8, 3)
      case 70 => (8, 4)
      case 71 => (8, 5)

      case 72 => (6, 6)
      case 73 => (6, 7)
      case 74 => (6, 8)
      case 75 => (7, 6)
      case 76 => (7, 7)
      case 77 => (7, 8)
      case 78 => (8, 6)
      case 79 => (8, 7)
      case 80 => (8, 8)

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

  // 0 1 2
  // 3 4 5
  // 6 7 8
  def findWin(sb: ArrayBuffer[Char]): Option[(Int, Int)] = {
    // X wins
    if (sb(0) == 'X' && sb(1) == 'X' && sb(2) == 'X') return Some((0, 2))
    if (sb(3) == 'X' && sb(4) == 'X' && sb(5) == 'X') return Some((3, 5))
    if (sb(6) == 'X' && sb(7) == 'X' && sb(8) == 'X') return Some((6, 8))

    if (sb(0) == 'X' && sb(3) == 'X' && sb(6) == 'X') return Some((0, 6))
    if (sb(1) == 'X' && sb(4) == 'X' && sb(7) == 'X') return Some((1, 7))
    if (sb(2) == 'X' && sb(5) == 'X' && sb(8) == 'X') return Some((2, 8))

    if (sb(0) == 'X' && sb(4) == 'X' && sb(8) == 'X') return Some((0, 8))
    if (sb(2) == 'X' && sb(4) == 'X' && sb(6) == 'X') return Some((2, 6))

    // O wins
    if (sb(0) == 'O' && sb(1) == 'O' && sb(2) == 'O') return Some((0, 2))
    if (sb(3) == 'O' && sb(4) == 'O' && sb(5) == 'O') return Some((3, 5))
    if (sb(6) == 'O' && sb(7) == 'O' && sb(8) == 'O') return Some((6, 8))

    if (sb(0) == 'O' && sb(3) == 'O' && sb(6) == 'O') return Some((0, 6))
    if (sb(1) == 'O' && sb(4) == 'O' && sb(7) == 'O') return Some((1, 7))
    if (sb(2) == 'O' && sb(5) == 'O' && sb(8) == 'O') return Some((2, 8))

    if (sb(0) == 'O' && sb(4) == 'O' && sb(8) == 'O') return Some((0, 8))
    if (sb(2) == 'O' && sb(4) == 'O' && sb(6) == 'O') return Some((2, 6))

    // No wins
    None
  }

  def count2s(sb: ArrayBuffer[Char], isX: Boolean) = {
    val xo = if (isX) 'X' else 'O'
    var total = 0

    if ((sb(0) == ' ' && sb(1) == xo && sb(2) == xo) || (sb(0) == xo && sb(1) == ' ' && sb(2) == xo) || (sb(0) == xo && sb(1) == xo && sb(2) == ' ')) total += 1
    if ((sb(3) == ' ' && sb(4) == xo && sb(5) == xo) || (sb(3) == xo && sb(4) == ' ' && sb(5) == xo) || (sb(3) == xo && sb(4) == xo && sb(5) == ' ')) total += 1
    if ((sb(6) == ' ' && sb(7) == xo && sb(8) == xo) || (sb(6) == xo && sb(7) == ' ' && sb(8) == xo) || (sb(6) == xo && sb(7) == xo && sb(8) == ' ')) total += 1

    if ((sb(0) == ' ' && sb(3) == xo && sb(6) == xo) || (sb(0) == xo && sb(3) == ' ' && sb(6) == xo) || (sb(0) == xo && sb(3) == xo && sb(6) == ' ')) total += 1
    if ((sb(1) == ' ' && sb(4) == xo && sb(7) == xo) || (sb(1) == xo && sb(4) == ' ' && sb(7) == xo) || (sb(1) == xo && sb(4) == xo && sb(7) == ' ')) total += 1
    if ((sb(2) == ' ' && sb(5) == xo && sb(8) == xo) || (sb(2) == xo && sb(5) == ' ' && sb(8) == xo) || (sb(2) == xo && sb(5) == xo && sb(8) == ' ')) total += 1

    if ((sb(0) == ' ' && sb(4) == xo && sb(8) == xo) || (sb(0) == xo && sb(4) == ' ' && sb(8) == xo) || (sb(0) == xo && sb(4) == xo && sb(8) == ' ')) total += 1
    if ((sb(2) == ' ' && sb(4) == xo && sb(6) == xo) || (sb(2) == xo && sb(4) == ' ' && sb(6) == xo) || (sb(2) == xo && sb(4) == xo && sb(6) == ' ')) total += 1

    total
  }

  def getMovesInSubboard(board: ArrayBuffer[ArrayBuffer[Char]], activeBoard: Int) = {
    val subboard = board(activeBoard)
    var moves = ArrayBuffer[(Int, Int)]()
    var sb = 0

    do {
      if (subboard(sb) == ' ') moves += ((activeBoard, sb))
      sb += 1
    } while (sb < subboard.size)

    moves
  }

  def emptyBoard = {
    var board = ArrayBuffer[ArrayBuffer[Char]]()

    val subboard = ArrayBuffer(' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ')
    for (i <- 1 to 9) board += subboard.clone

    board
  }

  def cloneBoard(board: ArrayBuffer[ArrayBuffer[Char]]) = {
    var boardClone = ArrayBuffer[ArrayBuffer[Char]]()
    board.foreach(boardClone += _.clone)
    boardClone
  }
}