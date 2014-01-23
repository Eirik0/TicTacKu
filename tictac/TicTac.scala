package tictac
import swing._
import Swing._
import event._
import PlayerMoves._
import Oracles._
import Game._
import Strategy._
import Draw._
import scala.collection.mutable.HashMap
import scala.concurrent.future
import scala.concurrent.ExecutionContext.Implicits._
import java.awt.Color
import java.awt.Font
import scala.collection.mutable.ArrayBuffer

object TicTac extends SimpleSwingApplication {
  var _game: TicTac = TicTacKu
  def game_=(game: TicTac): Unit = _game = game
  def game = _game

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

    private var _wonBoards = ArrayBuffer[Win]()
    def wonBoards_=(wonBoards: ArrayBuffer[Win]): Unit = _wonBoards = wonBoards
    def wonBoards = _wonBoards

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
      for (win <- wonBoards) {
        if (win.isPlayer1) g.setColor(p1Color) else g.setColor(p2Color)
        win.draw(g, ddx, ddy)
      }

      if (game.isP1Turn) g.setColor(p1Color) else g.setColor(p2Color)

      // Active Board Highlight
      if (!game.isGameOver) {
        if (game.activeBoard != -1) {
          val (activeBoardX, activeBoardY) = BoardUtils.getActiveBoardXY(game.activeBoard)
          drawThickRectangle(g, (activeBoardX * dx).toInt, (activeBoardY * dy).toInt, dx.toInt, dy.toInt)
        } else {
          drawThickRectangle(g, 1, 1, width - 2, height - 2)
        }
      }

      // Mouse Highlight
      if (drawMouse && !game.isGameOver && !game.isComputerThinking && !isComputerMatch) {
        val (sqX, sqY) = ((mouseX / ddx.toInt) * ddx.toInt, (mouseY / ddy.toInt) * ddy.toInt)
        val (b, sb) = BoardUtils.getBoardNM((mouseX / ddx).toInt, (mouseY / ddy).toInt)

        g.fillOval(mouseX, mouseY, (ddx / 2).toInt, (ddy / 2).toInt)

        if ((b == game.activeBoard || game.activeBoard == -1) && game.isLegalMove(b, sb)) {
          drawThickRectangle(g, sqX + guiSpacing / 2, sqY + guiSpacing / 2, ddx.toInt - guiSpacing, ddy.toInt - guiSpacing)
        }
      }

      // Special drawing if the game is over
      if (game.isGameOver) {
        val scale = scala.math.min(width, height) / 10
        g.setFont(new Font("Consolas", Font.BOLD, scale))

        if (game.doesP1Win) drawWin(g, "Blue Wins!!!", blendWinColor(p1Color), width, height)
        else if (game.doesP2Win) drawWin(g, "Red Wins!!!", blendWinColor(p2Color), width, height)
        else {
          val avg = new Color((p1Color.getRed + p2Color.getRed) / 2, (p1Color.getGreen + p2Color.getGreen) / 2, (p1Color.getBlue + p2Color.getBlue) / 2)
          drawWin(g, "Draw ...", blendWinColor(avg), width, height)
        }
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
      case MouseReleased(_, location, _, _, _) if !game.isGameOver => {
        val (b, sb) = BoardUtils.getBoardNM((9 * location.getX / peer.getWidth).toInt, (9 * location.getY / peer.getHeight).toInt)
        if (b == game.activeBoard || game.activeBoard == -1) {
          (game.player1, game.player2) match {
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

    var games = HashMap[String, TicTac]()
    def registerGame(g: TicTac) = games += (g.title -> g)

    registerGame(TicTacKu)
    registerGame(TicTacCon)

    val gamesList = games.keySet.toList.sortWith(_ > _) // So Ku comes first

    var players = HashMap[String, Player]()
    def registerPlayer(name: String, player: Player) = players += (name -> player)

    registerPlayer("Human", Human)
    registerPlayer("Rando", Computer(getRandomMove))
    registerPlayer("Nick Vanderbot", Computer(getMoveMinimax(FastWin, 6, _)))
    registerPlayer("Machiavelli", Computer(getMoveMinimax(PossibleWins, 6, _)))
    registerPlayer("Sandy", Computer(getMoveMinimax(MovesToWin, 5, _)))
    registerPlayer("JPlayer", Computer(getMovej))

    val playerList = players.keySet.toList.sortWith(_ < _) // So Human comes first

    background = Color.WHITE

    def setWins() = {
      if (game.doesP1Win) p1TotalWins += 1
      else if (game.doesP2Win) p2TotalWins += 1
      else totalDraws += 1
      winsLabel.text = "<HTML>" + col("blue") + " " + p1TotalWins + col("red") + " " + p2TotalWins + col("black") + " " + totalDraws + "</HTML>"
    }

    def setCurrentPlayer() = {
      if (game.isGameOver) {
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

    def getGame = games(gameBox.selection.item)

    val gameLabel = new Label("Game: ")
    val gameBox = new ComboBox(gamesList)
    val plLabel = new Label("<HTML>" + col("blue") + "<u>Player 1: </HTML>")
    val p1Box = new ComboBox(playerList)
    val p2Label = new Label("<HTML>" + col("red") + "Player 2: </HTML>")
    val p2Box = new ComboBox(playerList)
    val newGameButton = new Button("New Game!")
    val winsLabel = new Label("<HTML>" + col("blue") + " " + p1TotalWins + col("red") + " " + p2TotalWins + col("black") + " " + totalDraws + "</HTML>")

    contents += gameLabel
    contents += gameBox
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
    if (game.isLegalMove(b, sb)) {
      game.makeMove(b, sb) match {
        case Some(win) => ticTacPanel.wonBoards += win
        case None => {}
      }
      update()
      if (makeComputerMove) moveComputer()
    }
  }

  def moveComputer() = {
    Thread.sleep(100L)
    if (!game.isComputerThinking && !game.isGameOver) {
      val futureMove = future {
        game.makeComputerMove() match {
          case Some(win) => ticTacPanel.wonBoards += win
          case None => {}
        }
      }
      futureMove.onSuccess { case _ => update() }
    }
  }

  def update() {
    Swing.onEDT {
      if (game.isGameOver) { playerSelectionPanel.setWins() }
      else playerSelectionPanel.setCurrentPlayer()
      ticTacPanel.repaint
    }
  }

  /**
   * Main Frame
   */
  def top = new MainFrame {
    title = "Tic Tac"

    val gamePanel = new BorderPanel {
      // Add Panels
      add(playerSelectionPanel, BorderPanel.Position.North)
      add(ticTacPanel, BorderPanel.Position.Center)

      // Listen to the new game button on the player selection
      listenTo(playerSelectionPanel.newGameButton)
      reactions += {
        case ButtonClicked(`playerSelectionPanel`.newGameButton) => {
          game.reset()
          game = playerSelectionPanel.getGame
          isComputerMatch = false
          playerSelectionPanel.setCurrentPlayer()
          ticTacPanel.wonBoards.clear()
          ticTacPanel.repaint

          game.player1 = playerSelectionPanel.getPlayer1
          game.player2 = playerSelectionPanel.getPlayer2

          (game.player1, game.player2) match {
            case (Computer(_), Computer(_)) => { // Computer match starts here
              isComputerMatch = true
              val futureGame = future {
                do {
                  moveComputer()
                } while (!game.isGameOver)
              }
            }
            case (Computer(_), Human) if game.isP1Turn => moveComputer()
            case (Human, Computer(_)) if !game.isP1Turn => moveComputer()
            case _ => {} // Human v. Human handled elsewhere
          }
        }
      }
    }

    contents = gamePanel
  }
}