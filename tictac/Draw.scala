package tictac
import Game._
import java.awt.Graphics2D
import java.awt.Color

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

  def col(c: String) = "<font color = \"" + c + "\">"
}