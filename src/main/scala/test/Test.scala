package test

import java.awt._
import java.awt.event._
import javax.swing._
import gui._
import javafx.scene.input.MouseButton
import misc.{ImageInfo, ProjectInfo, RectangleInfo}
import layering.Layering

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import javax.swing.event.MouseInputAdapter
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class TestPane(var image_list: ListBuffer[ImageInfo]) extends JPanel {
  override def getPreferredSize = new Dimension(400, 200)

  var rectangle_list: ListBuffer[RectangleInfo] = ListBuffer()
  var orig_x = -1
  var orig_y = -1
  var dest_x = -1
  var dest_y = -1



  private def getOverlapingRectangles(rectangles: ListBuffer[RectangleInfo]): ListBuffer[ListBuffer[RectangleInfo]] = {
    var ret: ListBuffer[ListBuffer[RectangleInfo]] = ListBuffer()
    var rects: ListBuffer[RectangleInfo] = ListBuffer()
    rectangles.reverse.foreach(r => rects += r)

    while(!rects.isEmpty) {
      var group: ListBuffer[RectangleInfo] = ListBuffer()
      var queue: ListBuffer[RectangleInfo] = ListBuffer()
      queue += rects.remove(0)
      while(!queue.isEmpty) {
        val rect_0 = queue.remove(0)
        var rect_iter = rects.iterator
        var ind: Int = 0
        while (rect_iter.hasNext) {
          val rect_1 = rect_iter.next()
          val r0: Rectangle = new Rectangle(rect_0.orig_x, rect_0.orig_y, rect_0.dest_x - rect_0.orig_x, rect_0.dest_y - rect_0.orig_y)
          val r1: Rectangle = new Rectangle(rect_1.orig_x, rect_1.orig_y, rect_1.dest_x - rect_1.orig_x, rect_1.dest_y - rect_1.orig_y)
          if (r0.intersects(r1)) {
            queue += rect_1
            rects = rects.filter(r => r != rect_1)
          }
        }
        group += rect_0
      }
      ret += group
    }

    ret
  }

  private def makePolygon(rectangles: ListBuffer[RectangleInfo]): Polygon = {
    val points: ListBuffer[Point] = calculatePoints(rectangles)
    var polygon = new Polygon()
    points.foreach(p => {
      polygon.addPoint(p.x, p.y)
    })
    polygon
  }

  private def getAllYCoords(rectangles: ListBuffer[RectangleInfo]): mutable.Set[Float] = {
    val allTopYCoords: ListBuffer[Float] = rectangles.map(r => r.orig_y)
    val allBottomYCoords: ListBuffer[Float] = rectangles.map(r => r.dest_y)

    val allCoords: mutable.Set[Float] = mutable.HashSet()
    allTopYCoords.foreach(t => allCoords.add(t))
    allBottomYCoords.foreach(b => allCoords.add(b))
    allCoords
  }

  private def rectsAtYExcBottomLines(y: Float, rectangles: ListBuffer[RectangleInfo]): ListBuffer[RectangleInfo] = {
    rectangles.filter(r => r.orig_y <= y && r.dest_y > y)
  }

  private def rectsAtYIncBottomLines(y: Float, rectangles: ListBuffer[RectangleInfo]): ListBuffer[RectangleInfo] = {
    rectangles.filter(r => r.orig_y <= y && r.dest_y == y)
  }

  private def rectanglesAtY(y: Float, rectangles: ListBuffer[RectangleInfo]): ListBuffer[RectangleInfo] = {
    val rectsAtYExcBottomLines: ListBuffer[RectangleInfo] = this.rectsAtYExcBottomLines(y, rectangles)

    if (rectsAtYExcBottomLines.length > 0) {
      // there are rectangles that are not closing here, so ignore those that are closing.
      rectsAtYExcBottomLines
    }
    else {
      // there are only rectangle bottom lines so we need to consider them.
      this.rectsAtYIncBottomLines(y, rectangles)
    }
  }

  def minXLeftCoord(y: Float, rectangles: ListBuffer[RectangleInfo]): Float = {
    rectanglesAtY(y, rectangles).map(r => r.orig_x).sorted.head
  }

  def maxXRightCoord(y: Float, rectangles: ListBuffer[RectangleInfo]): Float = {
    rectanglesAtY(y, rectangles).map(r => r.dest_x).sorted.last
  }

  private def calculatePoints(rectangles: ListBuffer[RectangleInfo]): ListBuffer[Point] = {
    var ret: ListBuffer[Point] = ListBuffer()

    var yCoords: ListBuffer[Float] = ListBuffer()
    getAllYCoords(rectangles).toList.foreach(y => yCoords += y)
    yCoords = yCoords.sorted

    var previousLeftCoord: Float = 0
    var previousRightCoord: Float = 0

    yCoords.foreach(y => {
      println("Considering yCoords " + y)
      val minimumXLeftCoord = minXLeftCoord(y, rectangles)
      val maximumXRightCoord = maxXRightCoord(y, rectangles)
      println("min X: " + minimumXLeftCoord)
      println("max X: " + maximumXRightCoord)

      if (y == yCoords.head) {
        ret += new Point(minimumXLeftCoord.toInt, y.toInt)
        ret += new Point(maximumXRightCoord.toInt, y.toInt)
      } else {
        if (minimumXLeftCoord != previousLeftCoord) {
          ret = ret.prepend(new Point(previousLeftCoord.toInt, y.toInt))
          ret = ret.prepend(new Point(minimumXLeftCoord.toInt, y.toInt))
        }
        else
          ret = ret.prepend(new Point(minimumXLeftCoord.toInt, y.toInt))

        if (maximumXRightCoord != previousRightCoord) {
          ret += new Point(previousRightCoord.toInt, y.toInt)
          ret += new Point(maximumXRightCoord.toInt, y.toInt)
        }
        else
          ret += new Point(maximumXRightCoord.toInt, y.toInt)
      }

      previousLeftCoord = minimumXLeftCoord
      previousRightCoord = maximumXRightCoord
    })

    ret
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    image_list.foreach(i => {
      g.drawImage(i.image, 0, 0, null)
    })

    val rect_groups: ListBuffer[ListBuffer[RectangleInfo]] = getOverlapingRectangles(rectangle_list)

    rect_groups.foreach(rg => {
      val polygon: Polygon = makePolygon(rg)
      g.drawPolygon(polygon)
    })
  }

  def update_list(new_list: ListBuffer[ImageInfo]) = {
    this.image_list = new_list
  }

  def init(): Unit = {
    var pressed: MouseEvent = null
    this.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        pressed = e
        println("ORIGIN")
        orig_x = e.getX
        orig_y = e.getY

        println("x = " + e.getX)
        println("y = " + e.getY)
      }

      override def mouseReleased(e: MouseEvent): Unit = {
        pressed = null
        rectangle_list += new RectangleInfo(orig_x, orig_y, dest_x, dest_y)
        repaint()
      }
    })

    this.addMouseMotionListener(new MouseMotionAdapter {
      override def mouseDragged(e: MouseEvent): Unit = {
        dest_x = e.getX
        dest_y = e.getY

      }
    })
  }

  init()
}

object Test extends App {

  import javax.swing.JList

  var layer_number:Int = 0
  val listData: ListBuffer[String] = ListBuffer()

  var image_list: ListBuffer[ImageInfo] = ListBuffer()
  var image_listbox: JList[String] = new JList(listData.toArray)
  var test_pane: TestPane = new TestPane(image_list)

  var file_path: String = ""
  val fc = new JFileChooser()
  val frame = new JFrame("Scala Image Editing Tool")
  frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  frame.setSize(new Dimension(1200, 1000))


  def get_menu_bar(): JMenuBar = {
    val menu_bar = new JMenuBar()
    val file_menu = new JMenu("File")
    val new_project = new JMenuItem("New Project")
    val import_project = new JMenuItem("Import Project")
    val save_project = new JMenuItem("Save Project")
    new_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: NewProjectDialog = new NewProjectDialog(frame)
          dialog.setVisible(true)
          println(dialog.file_path)
          layer_number += 1
          image_list += new ImageInfo(dialog.file_path.split("\\\\").last, ImageIO.read(new File(dialog.file_path)), layer_number-1)
          println(image_list.last.toString)
          listData += ("Layer " + (layer_number-1) + ": " + dialog.file_path.split("\\\\").last)
          image_listbox.setListData(listData.toArray)
          test_pane.repaint()
          image_listbox.repaint()
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
          val info: ProjectInfo = new ProjectInfo(0, true) //PLACEHOLDER
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

    val options_menu = new JMenu("Options")
    val new_layer = new JMenuItem("New Layer")
    new_layer.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: NewLayerDialog = new NewLayerDialog(frame)
          dialog.setVisible(true)
          layer_number += 1
          image_list += new ImageInfo(dialog.file_path.split("\\\\").last, ImageIO.read(new File(dialog.file_path)), layer_number - 1)
          listData += ("Layer " + (layer_number-1) + ": " + dialog.file_path.split("\\\\").last)
          image_listbox.setListData(listData.toArray)
          test_pane.repaint()
          image_listbox.repaint()
        } catch {
          case e: Exception => println("Error while adding a new layer.")
        }
      }
    })
    options_menu.add(new_layer)

    menu_bar.add(file_menu)
    menu_bar.add(options_menu)
    menu_bar
  }

  private def update_image_list(new_list: ListBuffer[ImageInfo]) = {
    image_list.clear()
    new_list.foreach(i => image_list += i)
    image_list.sortBy(_.layer)
    image_list = image_list.reverse
    listData.clear()
    image_list.foreach(i => listData += ("Layer " + i.layer + ": " + i.name))
    image_listbox.setListData(listData.toArray)
    test_pane.update_list(image_list)
    test_pane.repaint()
  }

  private def init_listbox() = {
    image_listbox.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        if (SwingUtilities.isRightMouseButton(e)) {
          val menu = new JPopupMenu()
          val opacity_item = new JMenuItem("Change opacity")
          val move_up_item = new JMenuItem("Move layer up")
          val move_down_item = new JMenuItem("Move layer down")

          opacity_item.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              try {
                val dialog: ChangeOpacityDialog = new ChangeOpacityDialog(frame)
                dialog.setVisible(true)

                val opacity: Double = dialog.opacity
                println(opacity)
                if (opacity < 0)
                  throw new Exception("Error")

                val file_name = image_listbox.getSelectedValue.split(" ").last
                val new_list: ListBuffer[ImageInfo] = ListBuffer()
                image_list.foreach(i => {
                  if (i.name == file_name)
                    new_list += new ImageInfo(i.name, Layering.makeImageTranslucent(i.image, opacity), i.layer)
                  else
                    new_list += i
                })
                update_image_list(new_list.reverse)
              } catch {
                case e: Exception => println("Error while adding a new layer.")
              }
            }
          })

          move_up_item.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              val file_name = image_listbox.getSelectedValue.split(" ").last
              println(file_name)
              var selected_layer: Int = -1
              val new_list: ListBuffer[ImageInfo] = ListBuffer()
              image_list.foreach(i => {
                if (i.name == file_name) {
                  new_list += new ImageInfo(i.name, i.image, i.layer + 1)
                  selected_layer = i.layer + 1
                } else if (i.layer == selected_layer)
                  new_list += new ImageInfo(i.name, i.image, i.layer - 1)
                else
                  new_list += i
              })
              update_image_list(new_list)
            }
          })

          move_down_item.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              val file_name = image_listbox.getSelectedValue.split(" ").last
              var selected_layer: Int = -1
              val new_list: ListBuffer[ImageInfo] = ListBuffer()
              image_list.reverse.foreach(i => {
                if (i.name == file_name) {
                  new_list += new ImageInfo(i.name, i.image, i.layer - 1)
                  selected_layer = i.layer - 1
                } else if (i.layer == selected_layer)
                  new_list += new ImageInfo(i.name, i.image, i.layer + 1)
                else
                  new_list += i
              })
              update_image_list(new_list.reverse)
            }
          })

          menu.add(opacity_item)
          menu.add(move_up_item)
          menu.add(move_down_item)
          menu.show(image_listbox, e.getX, e.getY)
        }
      }
    })
  }


  frame.setLayout(new GridBagLayout())
  val gbc = new GridBagConstraints()
  frame.setJMenuBar(get_menu_bar)
  gbc.ipady = 1000
  gbc.ipadx = 1000
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.fill = GridBagConstraints.HORIZONTAL
  gbc.gridwidth = 3
  test_pane.setBorder(BorderFactory.createLineBorder(Color.black))
  frame.add(test_pane, gbc)
  val panel = new JPanel()
  panel.setBorder(BorderFactory.createLineBorder(Color.black))
  gbc.ipady = 1000
  gbc.ipadx = 200
  gbc.gridx = 3
  gbc.gridy = 0
  init_listbox()
  frame.add(image_listbox, gbc)
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)


}