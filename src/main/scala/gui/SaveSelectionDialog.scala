package gui

import java.awt._
import java.awt.event._
import javax.swing._

class SaveSelectionDialog(owner: JFrame) extends JDialog(owner, true) {
  var selection_name = ""

  private def init(): Unit = {
    setTitle("Save Selection")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(2, 2))
    val create_selection = new JButton("Finish")
    val selection_title = new JTextField("")

    create_selection.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        selection_name = selection_title.getText
        if (selection_name != "")
          dispose()
      }
    })

    add(selection_title)
    add(create_selection)
  }

  init()
}
