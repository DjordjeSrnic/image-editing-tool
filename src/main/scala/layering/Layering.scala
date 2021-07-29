package layering

import layering.Layering.makeImageTranslucent
import misc.ProjectInfo

import javax.swing._
import java.awt._
import java.awt.image._
import java.beans.Visibility
import java.net.URL
import javax.imageio._
import javax.imageio.ImageIO
import java.awt.Dimension
import java.awt.image.BufferedImage
import java.io.IOException
import javax.swing.JPanel
import java.awt.Dimension

class TestPane extends JPanel {
  override def getPreferredSize = new Dimension(800, 600)

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    var image = ImageIO.read(
      new URL("https://es.belpatt.fr/img/belpatt_races/Fox-Terrier-Poil-Dur.jpg"));

    var image1 = ImageIO.read(
      new URL("https://i.pinimg.com/originals/da/7a/ca/da7acad9006f8051de9365f927bf6f85.jpg"));

    image = makeImageTranslucent(image, 0.1)

    g.drawImage(image1, 0, 0, null)
    g.drawImage(image, 0, 0, null)
  }
}

object Layering {

  def makeImageTranslucent(source: BufferedImage, alpha: Double): BufferedImage = {
    val target = new BufferedImage(source.getWidth, source.getHeight, java.awt.Transparency.TRANSLUCENT)
    // Get the images graphics
    val g = target.createGraphics
    // Set the Graphics composite to Alpha
    g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha.toFloat))
    // Draw the image into the prepared reciver image
    g.drawImage(source, null, 0, 0)
    // let go of all system resources in this Graphics
    g.dispose()
    // Return the image
    target
  }

  def changeLayerPosition(info: ProjectInfo, change_type: Int) = {
    change_type match {
      case 1 => info.layer = info.layer + 1
      case -1 => info.layer = info.layer - 1
    }
  }

  def changeLayerVisibility(info: ProjectInfo, visibility: Boolean) = {
    info.visible = visibility
  }

  def main(args: Array[String]) = {

    var image = ImageIO.read(
      new URL("https://es.belpatt.fr/img/belpatt_races/Fox-Terrier-Poil-Dur.jpg"));

    var image1 = ImageIO.read(
      new URL("https://i.pinimg.com/originals/da/7a/ca/da7acad9006f8051de9365f927bf6f85.jpg"));

    image1 = makeImageTranslucent(image1, 1)


    val frame = new JFrame()
    frame.setTitle("stained_image")
    frame.setSize(image.getWidth, image.getHeight)
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)

    val panel = new JPanel(new FlowLayout())

    val label = new JLabel
    label.setOpaque(true)
    label.setIcon(new ImageIcon(image))
    val label1 = new JLabel
    label1.setOpaque(true)
    label1.setIcon(new ImageIcon(image1))


    //frame.getContentPane.add(label1, BorderLayout.CENTER)
    //frame.getContentPane.add(label, BorderLayout.CENTER)
    frame.add(new TestPane)
    frame.setLocationRelativeTo(null)
    frame.pack
    frame.setVisible(true)
  }

}
