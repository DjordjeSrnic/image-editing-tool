package gui

import misc.ImageInfo

import java.awt._
import java.awt.event._
import javax.swing._

class ImportProjectDialog(owner: JFrame) extends JDialog(owner, true) {

  private val fc: JFileChooser = new JFileChooser()
  private var project_name: String = ""
  var image_info: ImageInfo = null

  private def init() = {
    setTitle("Import Project")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(2, 2))
    val import_project = new JButton("Import Project")
    val finish = new JButton("Finished")
    val selected_project = new JLabel("")

    import_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setAlwaysOnTop(false)
        val returnVal = fc.showOpenDialog(null)
        if (returnVal == JFileChooser.APPROVE_OPTION) {
          val file = fc.getSelectedFile
          project_name = file.getPath.split("\\\\").last
          selected_project.setText(project_name)
          // TO DO
          // Check file validity and extract info from the file
        }
        setAlwaysOnTop(true)
      }
    })

    finish.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        dispose()
      }
    })

    add(import_project)
    add(finish)
    add(new JLabel("Project: "))
    add(selected_project)
  }

  init()
}
