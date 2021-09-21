package gui

import misc.{ImageInfo, Pixel, RectangleInfo, SelectionInfo}

import java.awt._
import java.awt.event._
import java.io.{BufferedReader, File, FileReader}
import javax.imageio.ImageIO
import javax.swing._
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class ImportProjectDialog(owner: JFrame) extends JDialog(owner, true) {

  private val fc: JFileChooser = new JFileChooser()
  var project_name = ""
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
        var curRectangle: RectangleInfo = null
        var cur_rectangle_list: ListBuffer[RectangleInfo] = new ListBuffer()
        var cur_group: ArrayBuffer[Pixel] = new ArrayBuffer()
        var cur_selection_name = ""
        var cur_selection_active = false
        project_name = br.readLine()

        curLine = br.readLine()
        while (curLine != null ) {
          if (curLine.split("/")(0) == "images") {
            val file_path = curLine
            val new_rgb = br.readLine().split(",").map(_.toInt)
            val orig_rgb = br.readLine().split(",").map(_.toInt)
            val image = ImageIO.read(new File(file_path))
            image.setRGB(0, 0, image.getWidth(), image.getHeight(), new_rgb, 0, image.getWidth())
            val img_info = new ImageInfo(file_path.split("/").last, image, layer_number)
            img_info.pixels.foreach(p => {
              val color = new Color(orig_rgb(p.y * img_info.image.getWidth() + p.x))
              img_info.orig_pixels(p.y * img_info.image.getWidth() + p.x) = new Pixel(p.x, p.y, color.getRed/255.0, color.getGreen/255.0, color.getBlue/255.0, color.getAlpha/255.0)
            })
            image_list += img_info
            listData += ("Layer " + layer_number + ": " + file_path.split("/").last)
            layer_number += 1
          }

          if (curLine.split("/")(0) == "selection") {
            cur_selection_name = curLine.split("/")(1)
            cur_selection_active = curLine.split("/")(2).toBooleanOption.getOrElse(false)
          }

          if (curLine.split("/")(0) == "rectangle") {
            val rect_str = curLine.split("/")(1).split("-")
            val orig_x = rect_str(0).toInt
            val orig_y = rect_str(1).toInt
            val dest_x = rect_str(2).toInt
            val dest_y = rect_str(3).toInt
            curRectangle = new RectangleInfo(orig_x, orig_y, dest_x, dest_y)
          }

          if (curLine == "image/") {
            cur_group = new ArrayBuffer[Pixel]()
          }

          if (curLine.split("/")(0) == "pixel") {
            val rect_str = curLine.split("/")(1).split("-")
            val x = rect_str(0).toInt
            val y = rect_str(1).toInt
            val R = rect_str(2).toDouble
            val G = rect_str(3).toDouble
            val B = rect_str(4).toDouble
            val A = rect_str(5).toDouble
            cur_group += new Pixel(x, y, R, G, B, A)
          }

          if (curLine == "pixel-end") {
            curRectangle.changed_pixels += cur_group
          }

          if (curLine == "image-end") {
            cur_rectangle_list += curRectangle
          }

          if (curLine == "rectangle-end") {
            curSelection = new SelectionInfo(cur_selection_name, null, cur_rectangle_list, cur_selection_active, false)
            selection_list += curSelection
            cur_rectangle_list = new ListBuffer[RectangleInfo]()
          }

          curLine = br.readLine()
        }

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
