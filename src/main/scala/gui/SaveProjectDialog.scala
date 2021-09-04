package gui

import misc.{ImageInfo, ProjectInfo, SelectionInfo}

import java.awt._
import java.awt.event._
import javax.swing._
import java.io._
import scala.collection.mutable.ListBuffer

class SaveProjectDialog(owner: JFrame, images: ListBuffer[ImageInfo], selections: ListBuffer[SelectionInfo]) extends JDialog(owner, true) {

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

        bw.write("------------------------Images------------------------")
        bw.newLine()
        images.foreach(i => {
          val path = "images/" + i.name
          val rgb = i.image.getRGB(0, 0, i.image.getWidth, i.image.getHeight, null, 0, i.image.getWidth)
          bw.write(path)
          bw.newLine()
          bw.write(rgb.toList.mkString(","))
        })

        bw.newLine()
        bw.write("------------------------Selections------------------------")
        bw.newLine()
        selections.foreach(s => {
          bw.write("selection/" + s.name)
          bw.newLine()
          s.rectangles.foreach(r => {
            val line = "rectangle/" + r.orig_x + "-" + r.orig_y + "-" + r.dest_x + "-" + r.dest_y
            bw.write(line)
            bw.newLine()
          })
          bw.write("------------------------Previous--------------------------")
          bw.newLine()
          s.previous_state.foreach(p => {
            val rgb = p.image.getRGB(0, 0, p.image.getWidth, p.image.getHeight, null, 0, p.image.getWidth)
            bw.write("images/" + p.name)
            bw.newLine()
            bw.write(rgb.toList.mkString(","))
            bw.newLine()
          })

          bw.write("------------------------New-------------------------------")
          bw.newLine()
          s.new_state.foreach(n => {
            val rgb = n.image.getRGB(0, 0, n.image.getWidth, n.image.getHeight, null, 0, n.image.getWidth)
            bw.write("images/" + n.name)
            bw.newLine()
            bw.write(rgb.toList.mkString(","))
            bw.newLine()
          })
        })

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

