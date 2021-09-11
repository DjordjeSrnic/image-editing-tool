package gui

import misc.{ImageInfo, MethodInfo, Pixel, PixelWrapper, SelectionInfo}

import java.awt.{GridLayout, Rectangle}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JDialog, JFrame, JPanel, JTextField}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CompositionPixelCalculatorDialog(var images: ListBuffer[ImageInfo], owner: JFrame, selections: ListBuffer[SelectionInfo] = null) extends JDialog(owner, true) {

  private def init(): Unit = {
    setTitle("Composition Calculator")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 800, 300)
    setLayout(new GridLayout(4, 1))

    val op_panel = new JPanel(new GridLayout(1, 11))
    val add = new JButton("+")
    val sub = new JButton("-")
    val inv_sub = new JButton(":-")
    val mul = new JButton("*")
    val div = new JButton("/")
    val inv_div = new JButton(":/")
    val power = new JButton("pow")
    val log = new JButton("log")
    val abs = new JButton("abs")
    val min = new JButton("min")
    val max = new JButton("max")

    val rgb_panel = new JPanel(new GridLayout(1, 4))
    val r_text = new JTextField("0.0")
    val g_text = new JTextField("0.0")
    val b_text = new JTextField("0.0")

    val add_op = new JButton("Add Operation")
    val finish = new JButton("Finish")

    add.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.+)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.+
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    sub.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.-)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.-
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    inv_sub.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.:-)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.:-
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    mul.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.*)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.*
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    div.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper./)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper./
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    inv_div.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.:/)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.:/
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    power.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.power)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.power
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    log.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.log)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.log
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    abs.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.abs)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.abs
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    min.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.min)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.min
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    max.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.wrapper.comp_sequence += p.wrapper.max)
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selections.foreach(s => {
                s.rectangles.foreach(ri => {
                  val nr = new Rectangle(ri.orig_x, ri.orig_y, ri.dest_x - ri.orig_x, ri.dest_y - ri.orig_y)
                  if (temp_rect.intersects(nr)) {
                    val x = if (nr.getX <= temp_rect.getX) temp_rect.x else nr.x

                    val y = if (nr.getY <= temp_rect.getY) temp_rect.y else nr.y


                    val width = if (nr.getX <= temp_rect.getX) nr.getX + nr.getWidth - temp_rect.getX
                    else if (nr.getX + nr.getWidth >= temp_rect.getX + temp_rect.getWidth) temp_rect.getX + temp_rect.getWidth - nr.getX
                    else nr.getWidth

                    val height = if (nr.getY <= temp_rect.getY) nr.getY + nr.getHeight - temp_rect.getY
                    else if (nr.getY + nr.getHeight  >= temp_rect.getY + temp_rect.getHeight) temp_rect.getY + temp_rect.getHeight - nr.getY
                    else nr.getHeight

                    ri.changed_pixels.clear()
                    for(ind_i <- y until y + height.toInt)
                      for(ind_j <- x until x + width.toInt) {
                        val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                        if (!changed_pixels.contains(p.x + "-" + p.y)) {
                          p.wrapper.comp_sequence += p.wrapper.max
                          val cp = p.clone()
                          ri.changed_pixels += p
                          changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                        }
                      }
                  }
                })
              })
            }
          }
        })
      }
    })

    op_panel.add(add)
    op_panel.add(sub)
    op_panel.add(inv_sub)
    op_panel.add(mul)
    op_panel.add(div)
    op_panel.add(inv_div)
    op_panel.add(power)
    op_panel.add(log)
    op_panel.add(abs)
    op_panel.add(min)
    op_panel.add(max)

    rgb_panel.add(r_text)
    rgb_panel.add(g_text)
    rgb_panel.add(b_text)

    finish.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val R = java.lang.Double.parseDouble(r_text.getText)
        val G = java.lang.Double.parseDouble(g_text.getText)
        val B = java.lang.Double.parseDouble(b_text.getText)

        if (selections == null) {
          images.foreach(i => {
            if (i.active) {
              i.pixels.foreach(p => {
                p.wrapper.composite(R, G, B)
              })
              i.update_image()
            }
          })
        } else {
          selections.foreach(s => {
            s.rectangles.foreach(r => {
              r.changed_pixels.foreach(p => {
                p.wrapper.composite(R, G, B)
              })
            })
          })
        }
        dispose()
      }
    })

    this.add(op_panel)
    this.add(rgb_panel)
    this.add(add_op)
    this.add(finish)
  }

  init()
}
