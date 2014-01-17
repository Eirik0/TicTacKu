package tictac

import swing._
import Swing._
import event._
import PlayerMoves._
import Oracles._
import Game._
import Rules._
import Draw._
import scala.collection.mutable.HashMap
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits._
import java.awt.Color
import java.awt.Font

object TicTac extends SimpleSwingApplication {
  val game = TicTacKu

  var _isComputerMatch = false
  def isComputerMatch_=(isComputerMatch: Boolean): Unit = _isComputerMatch = isComputerMatch
  def isComputerMatch = _isComputerMatch

  class TicTacPanel extends Panel {
    private val p1Color = Color.BLUE
    private val p2Color = Color.RED

    preferredSize = new Dimension(729, 729)

    private var _mouseX = 0
    def mouseX_=(mouseX: Int): Unit = _mouseX = mouseX
    def mouseX = _mouseX

    private var _mouseY = 0
    def mouseY_=(mouseY: Int): Unit = _mouseY = mouseY
    def mouseY = _mouseY

    private var _drawMouse = false
    def drawMouse_=(drawMouse: Boolean): Unit = _drawMouse = drawMouse
    def drawMouse = _drawMouse

    /**
     * Paint
     */
    override def paintComponent(g: Graphics2D) {
      val width = peer.getWidth
      val height = peer.getHeight
      val (dx, dy) = (width.toFloat / 3, height.toFloat / 3)
      val (ddx, ddy) = (dx / 3, dy / 3)

      // Background
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, width, height)

      // Main Board
      g.setColor(Color.BLACK)
      drawBoard(g, 0, 0, width, height)

      // Subboards
      g.setColor(Color.BLACK)
      for (x <- 0 to 2; y <- 0 to 2) drawBoard(g, (x * dx).toInt, (y * dy).toInt, dx.toInt, dy.toInt)

      // Xs & Os
      for (x <- 0 to 8; y <- 0 to 8) {
        val (n, m) = BoardUtils.getBoardNM(x, y)
        val c = game.board(n)(m)
        if (c != ' ') {
          if (c == 'X') g.setColor(p1Color) else g.setColor(p2Color)
          g.fillOval((x * ddx + ddx / 4).toInt, (y * ddy + ddy / 4).toInt, (ddx / 2).toInt, (ddy / 2).toInt)
        }
      }

      // Win lines
      for (win <- game.wonBoards.values) {
        if (win.isPlayer1) g.setColor(p1Color) else g.setColor(p2Color)
        val x0 = (win.x0 * ddx + ddx / 2).toInt
        val y0 = (win.y0 * ddy + ddy / 2).toInt
        val x1 = (win.x1 * ddx + ddx / 2).toInt
        val y1 = (win.y1 * ddy + ddy / 2).toInt

        if (x0 == x1) drawThickHLine(g, x0, y0, x1, y1)
        else if (0 == y1) drawThickVLine(g, x0, y0, x1, y1)
        else drawThickDLine(g, x0, y0, x1, y1)
      }

      if (game.isP1Turn) g.setColor(p1Color) else g.setColor(p2Color)

      // Active Board Highlight
      if (game.activeBoard != -1) {
        val (activeBoardX, activeBoardY) = BoardUtils.getActiveBoardXY(game.activeBoard)
        drawThickRectangle(g, (activeBoardX * dx).toInt, (activeBoardY * dy).toInt, dx.toInt, dy.toInt)
      } else {
        drawThickRectangle(g, 1, 1, width - 2, height - 2)
      }

      // Mouse Highlight
      if (drawMouse && !game.gameOver && !game.isComputerThinking && !isComputerMatch) {
        val (sqX, sqY) = ((mouseX / ddx.toInt) * ddx.toInt, (mouseY / ddy.toInt) * ddy.toInt)
        val (b, sb) = BoardUtils.getBoardNM((mouseX / ddx).toInt, (mouseY / ddy).toInt)

        g.fillOval(mouseX, mouseY, (ddx / 2).toInt, (ddy / 2).toInt)

        if (b == game.activeBoard || game.activeBoard == -1) drawThickRectangle(g, sqX + guiSpacing / 2, sqY + guiSpacing / 2, ddx.toInt - guiSpacing, ddy.toInt - guiSpacing)
      }

      // Special drawing if the game is over
      if (game.gameOver) {
        var text = "Wins!!!"
        val scale = scala.math.min(width, height) / 10
        font = new Font("Consolas", Font.BOLD, scale)
        g.setFont(font)

        if (game.isP1Winning) {
          g.setColor(p1Color); g.drawString("Blue", scale, height / 2)
          g.setColor(blendWinColor(p1Color))
        } else if (game.isP2Winning) {
          g.setColor(p2Color); g.drawString("Red", scale, height / 2)
          g.setColor(blendWinColor(p2Color))
        } else {
          val avg = new Color((p1Color.getRed + p2Color.getRed) / 2, (p1Color.getGreen + p2Color.getGreen) / 2, (p1Color.getBlue + p2Color.getBlue) / 2)
          g.setColor(blendWinColor(avg))
          text = "Draw..."
        }

        g.drawString(text, width / 2 - scale, height / 2)
      }
    }

    /**
     * Reactions
     */
    listenTo(mouse.moves, mouse.clicks)
    reactions += {
      // Mouse Moves
      case MouseMoved(_, location, _) => {
        mouseX = location.getX.toInt
        mouseY = location.getY.toInt
        incWinColor()
        repaint
      }
      case MouseDragged(_, location, _) => {
        mouseX = location.getX.toInt
        mouseY = location.getY.toInt
        incWinColor()
        repaint
      }
      case MouseEntered(_, _, _) => drawMouse = true
      case MouseExited(_, _, _) => {
        drawMouse = false
        repaint
      }
      // Mouse Clicks 
      case MouseReleased(_, location, _, _, _) if !game.gameOver => {
        val (b, sb) = BoardUtils.getBoardNM((9 * location.getX / peer.getWidth).toInt, (9 * location.getY / peer.getHeight).toInt)
        if (b == game.activeBoard || game.activeBoard == -1) {
          game.players match {
            case (Human, Human) => moveMaybe(b, sb, false)
            case (Human, Computer(_)) if game.isP1Turn => moveMaybe(b, sb, true)
            case (Computer(_), Human) if !game.isP1Turn => moveMaybe(b, sb, true)
            case _ => {}
          }
        }
      }
    }
  }

  /**
   * Player Selection Panel
   */
  val playerSelectionPanel = new FlowPanel {
    var p1TotalWins = 0
    var p2TotalWins = 0
    var totalDraws = 0

    var players = HashMap[String, Player]()
    def registerPlayer(name: String, player: Player) = players += (name -> player)

    registerPlayer("Human", Human)
    registerPlayer("Rando", Computer(getRandomMove))
    registerPlayer("Nick Vanderbot", Computer(getMoveMinimax(FastWin, _)))
    registerPlayer("JPlayer", Computer(getMovej))

    val playerList = players.keySet.toList.sortWith(_ < _) // So Human comes first

    background = Color.WHITE

    def setWins() = {
      if (game.isP1Winning) p1TotalWins += 1
      else if (game.isP2Winning) p2TotalWins += 1
      else totalDraws += 1
      winsLabel.text = "<HTML>" + col("blue") + " " + p1TotalWins + col("red") + " " + p2TotalWins + col("black") + " " + totalDraws + "</HTML>"
    }

    def setCurrentPlayer() = {
      if (game.gameOver) {
        plLabel.text = "<HTML>" + col("blue") + "Player 1: </HTML>"
        p2Label.text = "<HTML>" + col("red") + "Player 2: </HTML>"
      } else if (game.isP1Turn) {
        plLabel.text = "<HTML>" + col("blue") + "<u>Player 1: </HTML>"
        p2Label.text = "<HTML>" + col("red") + "Player 2: </HTML>"
      } else {
        plLabel.text = "<HTML>" + col("blue") + "Player 1: </HTML>"
        p2Label.text = "<HTML>" + col("red") + "<u>Player 2: </HTML>"
      }
    }

    def getPlayer(name: String): Player = players(name)

    def getPlayer1 = getPlayer(p1Box.selection.item)
    def getPlayer2 = getPlayer(p2Box.selection.item)

    val plLabel = new Label("<HTML>" + col("blue") + "<u>Player 1: </HTML>")
    val p1Box = new ComboBox(playerList)
    val p2Label = new Label("<HTML>" + col("red") + "Player 2: </HTML>")
    val p2Box = new ComboBox(playerList)
    val newGameButton = new Button("New Game!")
    val winsLabel = new Label("<HTML>" + col("blue") + " " + p1TotalWins + col("red") + " " + p2TotalWins + col("black") + " " + totalDraws + "</HTML>")

    contents += plLabel
    contents += p1Box
    contents += p2Label
    contents += p2Box
    contents += newGameButton
    contents += winsLabel
  }

  /**
   * Tic Tac Panel
   */
  val ticTacPanel = new TicTacPanel

  /**
   * Game-UI Interactions
   */
  def moveMaybe(b: Int, sb: Int, makeComputerMove: Boolean) = {
    if (game.isValidMove(b, sb)) {
      game.makeMove(b, sb)
      update()
      if (makeComputerMove) moveComputer()
    }
  }

  def moveComputer() = {
    if (!game.gameOver) {
      val futureMove = future { game.makeComputerMove() }
      futureMove.onSuccess { case _ => update() }
    }
  }

  def update() {
    Swing.onEDT {
      if (game.gameOver) { playerSelectionPanel.setWins() }
      else playerSelectionPanel.setCurrentPlayer()
      ticTacPanel.repaint
    }
  }

  /**
   * Main Frame
   */
  def top = new MainFrame {
    title = "Tic Tac Ku"

    val gamePanel = new BorderPanel {
      // Add Panels
      add(playerSelectionPanel, BorderPanel.Position.North)
      add(ticTacPanel, BorderPanel.Position.Center)

      // Listen to the new game button on the player selection
      listenTo(playerSelectionPanel.newGameButton)
      reactions += {
        case ButtonClicked(`playerSelectionPanel`.newGameButton) => {
          game.reset()
          isComputerMatch = false
          playerSelectionPanel.setCurrentPlayer()
          ticTacPanel.repaint

          game.player1 = playerSelectionPanel.getPlayer1
          game.player2 = playerSelectionPanel.getPlayer2

          game.players match {
            case (Computer(_), Computer(_)) => {
              isComputerMatch = true
              val futureGame = future {
                do {
                  moveComputer()
                  Thread.sleep(33L)
                } while (!game.gameOver)
              }
              futureGame.onSuccess { case _ => update() }
            }
            case (Computer(_), Human) if game.isP1Turn => moveComputer()
            case (_, Computer(_)) if !game.isP1Turn => moveComputer()
            case _ => {} // Human v. Human handled elsewhere
          }
        }
      }
    }

    contents = gamePanel
  }
}