package test

import java.awt._
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing._
import scala.io._
import gui._
import misc.ImageInfo

object Test extends App {

  var file_path: String = ""
  val fc = new JFileChooser()
  val frame = new JFrame("Scala Image Editing Tool")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setSize(new Dimension(1200, 800))


  def get_menu_bar(): JMenuBar = {
    val menu_bar = new JMenuBar()
    val file_menu = new JMenu("File")
    val new_project = new JMenuItem("New Project")
    val import_project = new JMenuItem("Import Project")
    val save_project = new JMenuItem("Save Project")
    new_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          /*val returnVal = fc.showOpenDialog(null)
          if (returnVal == JFileChooser.APPROVE_OPTION) {
            val file = fc.getSelectedFile
            file_path = file.getPath
            file_path = file_path.replace("\\", "\\\\")
            icon = new ImageIcon(file_path)
            label.setIcon(icon)
          }*/
          val dialog: NewProjectDialog = new NewProjectDialog(frame)
          dialog.setVisible(true)
          icon = new ImageIcon(dialog.file_path)
          label.setIcon(icon)
        } catch {
          case e: Exception => println("Error while creating a new project.")
        }
      }
    })

    import_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: ImportProjectDialog= new ImportProjectDialog(frame)
          dialog.setVisible(true)
          //TO DO
          //Get image info from dialog
        } catch {
          case e: Exception => println("Error while importing a project.")
        }
      }
    })

    save_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val info: ImageInfo = new ImageInfo() //PLACEHOLDER
          val dialog: SaveProjectDialog= new SaveProjectDialog(frame, info)
          dialog.setVisible(true)
          //TO DO
          //Get image info from dialog
        } catch {
          case e: Exception => println("Error while saving a project.")
        }
      }
    })
    file_menu.add(new_project)
    file_menu.add(import_project)
    file_menu.add(save_project)
    menu_bar.add(file_menu)
    menu_bar
  }

  import javax.swing.ImageIcon
  import javax.swing.JLabel


  var icon = new ImageIcon("")
  val label = new JLabel(icon)
  frame.setJMenuBar(get_menu_bar)
  frame.add(label)
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)


}