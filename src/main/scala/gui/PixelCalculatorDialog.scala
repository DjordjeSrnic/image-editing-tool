package gui

import misc.{ImageInfo, MethodInfo, Pixel, SelectionInfo}

import java.awt.{GridLayout, Rectangle}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JDialog, JFrame, JPanel, JTextField}
import scala.collection.mutable.ListBuffer

class PixelCalculatorDialog(var images: ListBuffer[ImageInfo], owner: JFrame, selections: ListBuffer[SelectionInfo] = null) extends JDialog(owner, true) {

  private def init(): Unit = {
    setTitle("Pixel Calculator")
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)

        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.+ , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.+ , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.+ , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)

        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.- , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.- , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.- , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.:- , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.:- , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.:- , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.* , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.* , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.* , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p./ , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p./ , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp./ , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.:/ , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.:/ , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.:/ , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.power , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.power , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.power , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.log , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.log , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.log , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.abs , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.abs , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.abs , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.min , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.min , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.min , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          if (i.active) {
            if (selections == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.max , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
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
                    i.pixels.foreach(p => if (p.x >= x && p.x < (x + width.toInt) && p.y >= y && p.y < (y + height.toInt)) {
                      p.op_sequence += new MethodInfo(p.max , (r, g, b))
                      val cp = p.clone()
                      cp.op_sequence += new MethodInfo(cp.max , (r, g, b))
                      ri.changed_pixels += cp
                    })
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
        images.foreach(i => {
          if (i.active) {
            i.pixels.foreach(p => {
              p.execute_methods()
            })
            i.update_image()
          }
        })

        selections.foreach(s => {
          s.rectangles.foreach(r => {
            val h = r.changed_pixels.head
            r.changed_pixels.foreach(p => {
              p.execute_methods()
            })
          })
        })
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
