package gui

import misc.ProjectInfo

import java.awt._
import java.awt.event._
import javax.swing._
import java.io._

class SaveProjectDialog(owner: JFrame, image_info: ProjectInfo) extends JDialog(owner, true) {

  private val fc: JFileChooser = new JFileChooser()
  val p: JDialog = this

  private def init() = {
    setTitle("Save Project")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(2, 2))
    fc.setDialogTitle("Specify a file to save");
    val save_project = new JButton("Save Project")
    val finish = new JButton("Finished")
    val project_path = new JLabel("")

    save_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setAlwaysOnTop(false)
        val returnVal = fc.showSaveDialog(p);
        if (returnVal == JFileChooser.APPROVE_OPTION) {
          val file = fc.getSelectedFile
          val path = file.getPath + ".info"
          project_path.setText(path)
        }
        setAlwaysOnTop(true)
      }
    })

    finish.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val file = new File(project_path.getText)
        val bw = new BufferedWriter(new FileWriter(file))
        bw.write("Hello, New World!")
        bw.close()
        dispose()
      }
    })

    add(save_project)
    add(finish)
    add(new JLabel("Path: "))
    add(project_path)
  }

  init()
}

