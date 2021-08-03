package gui

import java.awt._
import java.awt.event._
import javax.swing._

class SelectionColorDialog(owner: JFrame) extends JDialog(owner, true) {
  var color: Color = null

  private def init(): Unit = {
    setTitle("New Layer")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(2, 1))
    val color_chooser = new JColorChooser()
    val confirm_color = new JButton("Finish")

    confirm_color.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        color = color_chooser.getColor
        dispose()
      }
    })

    add(color_chooser)
    add(confirm_color)
  }

  init()
}
