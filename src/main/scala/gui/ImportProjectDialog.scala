package gui

import misc.{ImageInfo, ProjectInfo, RectangleInfo, SelectionInfo}

import java.awt._
import java.awt.event._
import java.io.{BufferedReader, File, FileReader}
import javax.imageio.ImageIO
import javax.swing._
import scala.collection.mutable.ListBuffer

class ImportProjectDialog(owner: JFrame) extends JDialog(owner, true) {

  private val fc: JFileChooser = new JFileChooser()
  private var project_name = ""
  var selected_file: File = null
  val image_list: ListBuffer[ImageInfo] = new ListBuffer()
  val selection_list: ListBuffer[SelectionInfo] = new ListBuffer()
  val listData: ListBuffer[String] = ListBuffer()
  var layer_number = 0

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
          selected_file = fc.getSelectedFile
          project_name = selected_file.getPath.split("\\\\").last
          selected_project.setText(project_name)
        }
        setAlwaysOnTop(true)
      }
    })

    finish.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val br = new BufferedReader(new FileReader(selected_file))
        var curLine: String = null
        var curSelection: SelectionInfo = null
        var reading_prev = false
        var reading_new = false
        val cur_rectangle_list: ListBuffer[RectangleInfo] = new ListBuffer()
        var cur_selection_name = ""
        curLine = br.readLine()
        var is_first_selection = true
        while (curLine != null ) {
          if (!reading_prev && !reading_new && curLine.split("/")(0) == "images") {
            val file_path = curLine
            val rgb = br.readLine().split(",").map(_.toInt)
            val image = ImageIO.read(new File(file_path))
            image.setRGB(0, 0, image.getWidth(), image.getHeight(), rgb, 0, image.getWidth())
            image_list +=  new ImageInfo(file_path.split("/").last, image, layer_number)
            listData += ("Layer " + layer_number + ": " + file_path.split("/").last)
            layer_number += 1
          }

          if (curLine.split("/")(0) == "selection") {
            if (cur_selection_name != "") {
              selection_list += curSelection
            }
            reading_prev = false
            reading_new = false
            cur_selection_name = curLine.split("/")(1)
            println(cur_selection_name)
          }

          if (curLine.split("/")(0) == "rectangle") {
            val rect_str = curLine.split("/")(1).split("-")
            val orig_x = rect_str(0).toInt
            val orig_y = rect_str(1).toInt
            val dest_x = rect_str(2).toInt
            val dest_y = rect_str(3).toInt
            cur_rectangle_list += new RectangleInfo(orig_x, orig_y, dest_x, dest_y)
          }

          if (curLine == "------------------------Previous--------------------------") {
            curSelection = new SelectionInfo(cur_selection_name, null, cur_rectangle_list, false)
            reading_prev = true
            reading_new = false
          }

          if (reading_prev && !is_first_selection && curLine.split("/")(0) == "images") {
            val file_path = curLine
            val rgb = br.readLine().split(",").map(_.toInt)
            val image = ImageIO.read(new File(file_path))
            image.setRGB(0, 0, image.getWidth(), image.getHeight(), rgb, 0, image.getWidth())
            curSelection.previous_state += new ImageInfo(file_path.split("\\\\").last, image, 0)
          } else if (reading_prev && !is_first_selection) {
            is_first_selection = false
          }

          if (curLine == "------------------------New-------------------------------") {
            reading_prev = false
            reading_new = true
          }

          if (reading_new && curLine.split("/")(0) == "images") {
            val file_path = curLine
            val rgb = br.readLine().split(",").map(_.toInt)
            val image = ImageIO.read(new File(file_path))
            image.setRGB(0, 0, image.getWidth(), image.getHeight(), rgb, 0, image.getWidth())
            curSelection.new_state += new ImageInfo(file_path.split("\\\\").last, image, 0)
          }
          curLine = br.readLine()
        }
        selection_list += curSelection
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
