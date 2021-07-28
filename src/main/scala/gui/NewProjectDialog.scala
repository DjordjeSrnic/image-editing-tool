package gui

import java.awt._
import java.awt.event._
import javax.swing._

class NewProjectDialog(owner: JFrame) extends JDialog(owner, true) {

  private val fc: JFileChooser = new JFileChooser()
  var file_path = ""
  private var file_name = ""

  private def init(): Unit = {
    setTitle("New Project")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(3, 2))
    val select_image = new JButton("Select Image")
    val create_project = new JButton("Finish")
    val project_name = new JTextField("")
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
        var error_dialog: JDialog = new JDialog()
        error_dialog.setTitle("Error")
        error_dialog.setBounds(500, 400, 500, 150)
        error_dialog.setAlwaysOnTop(true)
        error_dialog.setModal(true)
        file_name = project_name.getText()
        if (file_name == "") {
          setAlwaysOnTop(false)
          val error_label = new JLabel("The project name is empty.", SwingConstants.CENTER)
          error_label.setForeground(Color.RED)
          error_dialog.add(error_label)
          error_dialog.setVisible(true)
        }
        else if (file_path =="") {
          setAlwaysOnTop(false)
          val error_label = new JLabel("Please select an image.", SwingConstants.CENTER)
          error_label.setForeground(Color.RED)
          error_dialog.add(error_label)
          error_dialog.setVisible(true)
        }
        else {
          dispose()
        }
      }
    })

    add(select_image)
    add(create_project)
    add(new JLabel("File: "))
    add(image_file)
    add(new JLabel("Project Name: "))
    add(project_name)
  }

  init()
}
