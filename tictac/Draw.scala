package tictac
import Game._
import java.awt.Graphics2D
import java.awt.Color
import java.awt.FontMetrics

object Draw {
  val guiSpacing = 10

  var _winColor = 0 // up to 16777215
  def winColor_=(winColor: Int): Unit = _winColor = winColor
  def winColor = _winColor

  def incWinColor() = { winColor += 253; if (winColor > 1677215) winColor -= 1677215 }

  // Drawing Helpers
  def drawThickVLine(g: Graphics2D, x0: Int, y0: Int, x1: Int, y1: Int) {
    g.drawLine(x0 - 1, y0 - 1, x1 - 1, y1 - 1)
    g.drawLine(x0, y0, x1, y1)
    g.drawLine(x0 + 1, y0 + 1, x1 + 1, y1 + 1)
  }

  def drawThickHLine(g: Graphics2D, x0: Int, y0: Int, x1: Int, y1: Int) {
    g.drawLine(x0 - 1, y0 - 1, x1 - 1, y1 - 1)
    g.drawLine(x0, y0, x1, y1)
    g.drawLine(x0 + 1, y0 + 1, x1 + 1, y1 + 1)
  }

  def drawThickDLine(g: Graphics2D, x0: Int, y0: Int, x1: Int, y1: Int) {
    g.drawLine(x0 + 1, y0, x1 + 1, y1)
    g.drawLine(x0, y0, x1, y1)
    g.drawLine(x0, y0 + 1, x1, y1 + 1)
  }

  def drawThickRectangle(g: Graphics2D, x0: Int, y0: Int, width: Int, height: Int) = {
    g.drawRect(x0 - 1, y0 + 1, width, height)
    g.drawRect(x0, y0, width, height)
    g.drawRect(x0 + 1, y0 - 1, width, height)
  }

  def drawBoard(g: Graphics2D, x0: Int, y0: Int, width: Int, height: Int) {
    drawThickHLine(g, x0 + width / 3, y0 + guiSpacing, x0 + width / 3, y0 + height - guiSpacing)
    drawThickHLine(g, x0 + 2 * width / 3, y0 + guiSpacing, x0 + 2 * width / 3, y0 + height - guiSpacing)
    drawThickVLine(g, x0 + guiSpacing, y0 + height / 3, x0 + width - guiSpacing, y0 + height / 3)
    drawThickVLine(g, x0 + guiSpacing, y0 + 2 * height / 3, x0 + width - guiSpacing, y0 + 2 * height / 3)
  }

  def blendWinColor(c1: Color) = {
    val winRed = winColor % 256
    val winGreen = (winColor / 256) % 256
    val winBlue = (winColor / 65536) % 256

    new Color((c1.getRed + winRed) % 256, (c1.getGreen + winGreen) % 256, (c1.getBlue + winBlue) % 256)
  }

  def drawWin(g: Graphics2D, text: String, c: Color, width: Int, height: Int) = {
    g.setColor(blendWinColor(c))
    g.drawString(text, (width - g.getFontMetrics().stringWidth(text)) / 2, height / 2)
  }

  def col(c: String) = "<font color = \"" + c + "\">"

  /**
   * The Win class is used by the gui and represents how to draw that particular win on the board.
   */
  sealed abstract class Win {
    def draw(g: Graphics2D, ddx: Float, ddy: Float): Unit
    def isPlayer1: Boolean
  }

  case class WinKu(x0: Int, y0: Int, x1: Int, y1: Int, isP1: Boolean) extends Win {
    def draw(g: Graphics2D, ddx: Float, ddy: Float) {
      val dx0 = (x0 * ddx + ddx / 2).toInt
      val dy0 = (y0 * ddy + ddy / 2).toInt
      val dx1 = (x1 * ddx + ddx / 2).toInt
      val dy1 = (y1 * ddy + ddy / 2).toInt
      if (dx0 == dx1) drawThickHLine(g, dx0, dy0, dx1, dy1)
      else if (dy0 == dy1) drawThickVLine(g, dx0, dy0, dx1, dy1)
      else drawThickDLine(g, dx0, dy0, dx1, dy1)
    }
    def isPlayer1 = isP1
  }

  case class WinCon(board: Int, isP1: Boolean) extends Win {
    def draw(g: Graphics2D, ddx: Float, ddy: Float) {
      val (activeBoardX, activeBoardY) = BoardUtils.getActiveBoardXY(board)
      val dx = ddx * 3
      val dy = ddy * 3
      g.fillOval((activeBoardX * dx + 3 * ddx / 4).toInt, (activeBoardY * dy + 3 * ddy / 4).toInt, (dx / 2).toInt, (dy / 2).toInt)
    }
    def isPlayer1 = isP1
  }
}