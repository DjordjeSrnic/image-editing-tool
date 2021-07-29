package gui

import java.awt.event._
import java.awt._
import javax.swing._

class NewLayerDialog(owner: JFrame) extends JDialog(owner, true) {
  private val fc: JFileChooser = new JFileChooser()
  var file_path = ""
  private var file_name = ""

  private def init(): Unit = {
    setTitle("New Layer")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(2, 2))
    val select_image = new JButton("Select Image")
    val create_project = new JButton("Finish")
    val image_file = new JLabel("")


    select_image.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setAlwaysOnTop(false)
        val returnVal = fc.showOpenDialog(null)
        if (returnVal == JFileChooser.APPROVE_OPTION) {
          val file = fc.getSelectedFile
          file_path = file.getPath
          file_path = file_path.replace("\\", "\\\\")
          image_file.setText(file_path.split("\\\\").last)
        }
        setAlwaysOnTop(true)
      }
    })

    create_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        dispose()
      }
    })

    add(select_image)
    add(create_project)
    add(new JLabel("File: "))
    add(image_file)
  }

  init()
}
