package gui

import java.awt.{Color, GridLayout}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JColorChooser, JDialog, JFrame, JTextField}

class SelectNDialog(owner: JFrame) extends JDialog(owner, true) {
  var N: Int = 0

  private def init(): Unit = {
    setTitle("Select N")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 200, 200)
    setLayout(new GridLayout(2, 1))
    val n = new JTextField()
    val confirm_color = new JButton("Finish")

    confirm_color.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        N = n.getText.toInt
        dispose()
      }
    })

    add(n)
    add(confirm_color)
  }

  init()
}
