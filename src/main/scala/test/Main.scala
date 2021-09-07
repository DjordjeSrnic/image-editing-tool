package test

import java.awt._
import java.awt.event._
import javax.swing._
import gui._
import misc._

import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable._
import scala.collection.parallel.CollectionConverters._


object Main extends App {

  var layer_number:Int = 0
  var listData: ListBuffer[String] = ListBuffer()

  var image_list: ListBuffer[ImageInfo] = ListBuffer()
  var image_listbox: JList[String] = new JList(listData.toArray)
  var selection_list: ListBuffer[SelectionInfo] = ListBuffer()
  var selection_listbox: JList[String] = new JList(listData.toArray)
  var test_pane: EditingCanvas = new EditingCanvas(image_list)
  var activeSelection: SelectionInfo = null

  var file_path: String = ""
  val fc = new JFileChooser()
  val frame = new JFrame("Scala Image Editing Tool")
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(new Dimension(1430, 1070))


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
          layer_number += 1
          image_list += new ImageInfo(dialog.file_path.split("\\\\").last, ImageIO.read(new File(dialog.file_path)), layer_number-1)
          listData += ("Layer " + (layer_number-1) + ": " + dialog.file_path.split("\\\\").last)
          image_listbox.setListData(listData.toArray)
          test_pane.changed = true
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
          image_list = dialog.image_list
          selection_list = dialog.selection_list
          listData = dialog.listData
          selection_listbox.setListData(selection_list.map(_.name).toArray)
          image_listbox.setListData(listData.toArray)
          image_listbox.repaint()
          test_pane.update_list(image_list)
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while importing a project.")
        }
      }
    })

    save_project.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: SaveProjectDialog= new SaveProjectDialog(frame, image_list, selection_list)
          dialog.setVisible(true)
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
    val new_selection = new JMenuItem("New Selection")
    val save_selection = new JMenuItem("Save Selection")
    val color_selection = new JMenuItem("Color Selection")
    new_layer.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: NewLayerDialog = new NewLayerDialog(frame)
          dialog.setVisible(true)
          layer_number += 1
          image_list += new ImageInfo(dialog.file_path.split("\\\\").last, ImageIO.read(new File(dialog.file_path)), layer_number - 1)
          listData += ("Layer " + (layer_number-1) + ": " + dialog.file_path.split("\\\\").last)
          image_listbox.setListData(listData.toArray)
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while adding a new layer.")
        }
      }
    })
    new_selection.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        test_pane.rectangle_list.clear()
      }
    })
    save_selection.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: SaveSelectionDialog = new SaveSelectionDialog(frame)
          dialog.setVisible(true)
          selection_list += new SelectionInfo(dialog.selection_name, null, test_pane.rectangle_list.clone(), false)
          selection_listbox.setListData(selection_list.map(_.name).toArray)
          test_pane.rectangle_list.clear()
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while adding a new selection.")
        }
      }
    })
    color_selection.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          val dialog: SelectColorDialog = new SelectColorDialog(frame)
          dialog.setVisible(true)
          val color = dialog.color
          test_pane.rectangle_list.foreach(r => {
            r.color = color.getRGB
          })
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while adding a new layer.")
        }
      }
    })
    options_menu.add(new_layer)
    options_menu.add(new_selection)
    options_menu.add(save_selection)
    options_menu.add(color_selection)

    val edit_menu = new JMenu("Edit")
    val to_grayscale = new JMenuItem("To Grayscale")
    val to_negative = new JMenuItem("To Negative")
    val to_median_filter = new JMenuItem("To Median Filter")
    val to_weighted_median_filter = new JMenuItem("To Weighted Median Filter")
    val calculator = new JMenuItem("Open Calculator")
    to_grayscale.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          if (activeSelection.rectangles.isEmpty) {
            image_list.par.foreach(i => {
              if (i.active) {
                i.pixels.par.foreach(p => p.grayscale())
                i.update_image()
              }
            })
          } else {
            image_list.par.foreach(i => {
              if (i.active) {
                val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
                activeSelection.previous_state += i.copy()
                activeSelection.rectangles.foreach(ri => {
                  val r = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(r)) {
                    val x = if (r.getX <= temp_rect.getX) temp_rect.x else r.x

                    val y = if (r.getY <= temp_rect.getY) temp_rect.y else r.y

                    val width = if (r.getX <= temp_rect.getX) r.getX + r.getWidth - temp_rect.getX
                    else if (r.getX + r.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - r.getX
                    else r.getWidth

                    val height = if (r.getY <= temp_rect.getY) r.getY + r.getHeight - temp_rect.getY
                    else if (r.getY + r.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - r.getY
                    else r.getHeight

                    i.pixels.par.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) p.grayscale())
                  }
                })
              }
            })
          }
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while adding applying grayscale.")
        }
      }
    })
    to_negative.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          if (activeSelection.rectangles.isEmpty) {
            image_list.par.foreach(i => {
              if (i.active) {
                i.pixels.par.foreach(p => p.negative())
                i.update_image()
              }
            })
          } else {
            image_list.par.foreach(i => {
              if (i.active) {
                val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
                activeSelection.previous_state += i.copy()
                activeSelection.rectangles.foreach(ri => {
                  val r = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(r)) {
                    val x = if (r.getX <= temp_rect.getX) temp_rect.x else r.x

                    val y = if (r.getY <= temp_rect.getY) temp_rect.y else r.y

                    val width = if (r.getX <= temp_rect.getX) r.getX + r.getWidth - temp_rect.getX
                    else if (r.getX + r.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - r.getX
                    else r.getWidth

                    val height = if (r.getY <= temp_rect.getY) r.getY + r.getHeight - temp_rect.getY
                    else if (r.getY + r.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - r.getY
                    else r.getHeight

                    i.pixels.par.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) p.negative())
                  }
                })
                i.update_image()
                activeSelection.new_state += i.copy()
              }
            })
          }
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while applying negative.")
        }
      }
    })
    to_median_filter.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val dialog: SelectNDialog = new SelectNDialog(frame)
        dialog.setVisible(true)
        val N = dialog.N

        try {
          if (activeSelection == null || activeSelection.rectangles.isEmpty) {
            image_list.par.foreach(i => {
              if (i.active) {
                i.pixels.par.foreach(p => {
                  p.median_filter(i.pixels, N, i.image.getWidth, i.image.getHeight)
                })
                i.update_image()
              }
            })
          } else {
            image_list.par.foreach(i => {
              if (i.active) {
                val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
                activeSelection.previous_state += i.copy()
                activeSelection.rectangles.par.foreach(ri => {
                  val r = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(r)) {
                    val x = if (r.getX <= temp_rect.getX) temp_rect.x else r.x

                    val y = if (r.getY <= temp_rect.getY) temp_rect.y else r.y


                    val width = if (r.getX <= temp_rect.getX) r.getX + r.getWidth - temp_rect.getX
                    else if (r.getX + r.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - r.getX
                    else r.getWidth

                    val height = if (r.getY <= temp_rect.getY) r.getY + r.getHeight - temp_rect.getY
                    else if (r.getY + r.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - r.getY
                    else r.getHeight

                    i.pixels.par.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt))
                      p.median_filter(i.pixels, N, i.image.getWidth, i.image.getHeight))
                  }
                })
                i.update_image()
                activeSelection.new_state += i.copy()
              }
            })
          }
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception =>  {
            println(e)
            println("Error while applying median filter.")
          }
        }
      }
    })
    to_weighted_median_filter.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val dialog: SelectNDialog = new SelectNDialog(frame)
        dialog.setVisible(true)
        val N = dialog.N

        val matrix_dialog: SelectMatrixDialog = new SelectMatrixDialog(frame, N)
        matrix_dialog.setVisible(true)
        val weights = matrix_dialog.matrix.clone().toArray

        try {
          if (activeSelection == null || activeSelection.rectangles.isEmpty) {
            image_list.par.foreach(i => {
              if (i.active) {
                i.pixels.par.foreach(p => p.weighted_filter(i.pixels, weights, N, i.image.getWidth, i.image.getHeight))
                i.update_image()
              }
            })
          } else {
            image_list.par.foreach(i => {
              if (i.active) {
                val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
                i.update_image()
                activeSelection.previous_state += i.copy()
                activeSelection.rectangles.par.foreach(ri => {
                  val r = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(r)) {
                    val x = if (r.getX <= temp_rect.getX) temp_rect.x else r.x

                    val y = if (r.getY <= temp_rect.getY) temp_rect.y else r.y


                    val width = if (r.getX <= temp_rect.getX) r.getX + r.getWidth - temp_rect.getX
                    else if (r.getX + r.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - r.getX
                    else r.getWidth

                    val height = if (r.getY <= temp_rect.getY) r.getY + r.getHeight - temp_rect.getY
                    else if (r.getY + r.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - r.getY
                    else r.getHeight

                    i.pixels.par.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt))
                      p.weighted_filter(i.pixels, weights, N, i.image.getWidth, i.image.getHeight))
                  }
                })
                i.update_image()
                activeSelection.new_state += i.copy()
              }
            })
          }
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while applying weighted median filter.")
        }
      }
    })
    calculator.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        try {
          if (activeSelection == null || activeSelection.rectangles.isEmpty) {
            val calculator_dialog: PixelCalculatorDialog = new PixelCalculatorDialog(image_list, frame)
            calculator_dialog.setVisible(true)
          } else {
            val calculator_dialog: PixelCalculatorDialog = new PixelCalculatorDialog(image_list, frame, activeSelection)
            calculator_dialog.setVisible(true)
          }
          test_pane.changed = true
          test_pane.repaint()
        } catch {
          case e: Exception => println("Error while applying calculator .")
        }
      }
    })
    edit_menu.add(to_grayscale)
    edit_menu.add(to_negative)
    edit_menu.add(to_median_filter)
    edit_menu.add(to_weighted_median_filter)
    edit_menu.add(calculator)

    menu_bar.add(file_menu)
    menu_bar.add(options_menu)
    menu_bar.add(edit_menu)
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
    test_pane.changed = true
    test_pane.repaint()
  }

  private def init_image_listbox() = {
    image_listbox.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        val file_name = image_listbox.getSelectedValue.split(" ").last
        val selectedValue = image_list.find(i => i.name == file_name).head
        if (SwingUtilities.isRightMouseButton(e)) {
          val menu = new JPopupMenu()
          val opacity_item = new JMenuItem("Change opacity")
          val change_status = if (selectedValue.active) new JMenuItem("Make inactive") else new JMenuItem("Make active")
          val move_up_item = new JMenuItem("Move layer up")
          val move_down_item = new JMenuItem("Move layer down")

          opacity_item.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              try {
                val dialog: ChangeOpacityDialog = new ChangeOpacityDialog(frame)
                dialog.setVisible(true)

                val opacity: Double = dialog.opacity

                val new_list: ListBuffer[ImageInfo] = ListBuffer()
                image_list.reverse.foreach(i => {
                  if (i.name == file_name) {
                    i.pixels.foreach(p => p.set_opacity(opacity))
                    i.opacity = opacity
                    i.update_image()
                    new_list += i
                  } else
                    new_list += i
                })
                update_image_list(new_list)
              } catch {
                case e: Exception => println("Error while changing opacity.")
              }
            }
          })

          change_status.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              try {
                val file_name = image_listbox.getSelectedValue.split(" ").last
                val found = image_list.find(i => i.name == file_name).head
                found.active = !found.active

                val new_list: ListBuffer[ImageInfo] = ListBuffer()
                image_list.foreach(i => new_list += i)
                update_image_list(new_list)
                image_listbox.repaint()
              } catch {
                case e: Exception => println("Error while changing layer status.")
              }
            }
          })

          move_up_item.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              val file_name = image_listbox.getSelectedValue.split(" ").last
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
          menu.add(change_status)
          menu.add(move_up_item)
          menu.add(move_down_item)
          menu.show(image_listbox, e.getX, e.getY)
        }
      }
    })
  }

  private def init_selection_listbox() = {
    selection_listbox.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        val selection_name = selection_listbox.getSelectedValue
        val selectedValue = selection_list.find(s => s.name == selection_name).head
        if (SwingUtilities.isRightMouseButton(e)) {
          val menu = new JPopupMenu()
          val remove = new JMenuItem("Remove")
          val change_status = if (selectedValue.active) new JMenuItem("Make inactive") else new JMenuItem("Make active")

          remove.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              try {
                if (selectedValue.active) {
                  val previous_state = selectedValue.previous_state

                  previous_state.foreach(ps => {
                    val found = image_list.find(i => i.name == ps.name).head
                    found.image = ps.image
                    found.pixels = ps.pixels
                    found.opacity = ps.opacity
                    found.update_image()
                  })
                  activeSelection = null
                }

                selection_list = selection_list.filter(_.name != selectedValue.name)
                selection_listbox.setListData(selection_list.map(_.name).toArray)
                test_pane.changed = true
                test_pane.repaint()
              } catch {
                case e: Exception => println("Error while removing selection.")
              }
            }
          })

          change_status.addActionListener(new ActionListener() {
            def actionPerformed(e: ActionEvent): Unit = {
              try {
                if (selectedValue.active) {
                  val previous_state = activeSelection.previous_state
                  previous_state.foreach(ps => {
                    val found = image_list.find(i => i.name == ps.name).head
                    found.image = ps.image
                    found.pixels = ps.pixels.clone()
                    found.opacity = ps.opacity
                    found.update_image()
                  })
                  selectedValue.active = !selectedValue.active
                  activeSelection = null
                } else {
                  if (activeSelection != null) {
                    val previous_state = activeSelection.previous_state
                    previous_state.foreach(ps => {
                      val found = image_list.find(i => i.name == ps.name).head
                      found.image = ps.image
                      found.pixels = ps.pixels.clone()
                      found.opacity = ps.opacity
                      found.update_image()
                    })
                    val last_selection = selection_list.last
                    selection_list.foreach(s => {
                      if (s != last_selection) {
                        s.previous_state.clear()
                        last_selection.previous_state.foreach(p => s.previous_state += p)
                      }
                    })
                  }

                  val new_state = selectedValue.new_state
                  new_state.foreach(ns => {
                    val found = image_list.find(i => i.name == ns.name).head
                    found.image = ns.image
                    found.pixels = ns.pixels.clone()
                    found.opacity = ns.opacity
                    found.update_image()
                  })
                  selection_list.foreach(s => {
                    if (s.name != selectedValue.name)
                      s.active = false
                  })
                  selectedValue.active = !selectedValue.active
                  activeSelection = selectedValue
                }

                test_pane.changed = true
                test_pane.repaint()
              } catch {
                case e: Exception => println("Error while changing selection status.")
              }
            }
          })

          menu.add(remove)
          menu.add(change_status)
          menu.show(selection_listbox, e.getX, e.getY)
        }
      }
    })
  }


  frame.setLayout(new GridBagLayout())
  val gbc = new GridBagConstraints()
  frame.setJMenuBar(get_menu_bar)
  gbc.ipady = 1000
  gbc.ipadx = 1200
  gbc.gridx = 0
  gbc.gridy = 0
  gbc.fill = GridBagConstraints.HORIZONTAL
  gbc.gridwidth = 3
  test_pane.setBorder(BorderFactory.createLineBorder(Color.black))
  frame.add(test_pane, gbc)
  gbc.ipady = 1000
  gbc.ipadx = 200
  gbc.gridx = 3
  gbc.gridy = 0
  gbc.gridwidth = 1
  init_image_listbox()
  init_selection_listbox()
  image_listbox.setBorder(BorderFactory.createLineBorder(Color.black))
  selection_listbox.setBorder(BorderFactory.createLineBorder(Color.black))
  val list_panel = new JPanel(new GridLayout(2, 1))
  list_panel.add(image_listbox)
  list_panel.add(selection_listbox)
  frame.add(list_panel, gbc)
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)


}