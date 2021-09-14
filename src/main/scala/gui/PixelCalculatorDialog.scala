package gui

import misc.{ImageInfo, MethodInfo, Pixel, SelectionInfo}

import java.awt.{GridLayout, Rectangle}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JDialog, JFrame, JPanel, JSpinner, JTextField, SpinnerNumberModel}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class PixelCalculatorDialog(var images: ListBuffer[ImageInfo], owner: JFrame, selection: SelectionInfo = null) extends JDialog(owner, true) {

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
    val r_text = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1.0, 0.1))
    val g_text = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1.0, 0.1))
    val b_text = new JSpinner(new SpinnerNumberModel(0.0, 0.0, 1.0, 0.1))

    val finish = new JButton("Finish")

    add.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.+ , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.+ , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    sub.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.- , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.- , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    inv_sub.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.:- , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.:- , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    mul.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.* , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.* , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    div.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p./ , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p./ , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    inv_div.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.:/ , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.:/ , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    power.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.power , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.power , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    log.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.log , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.log , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    abs.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.abs , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.abs , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    min.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.min , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.min , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
        })
      }
    })

    max.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getValue + "")
        val g = java.lang.Double.parseDouble(g_text.getValue + "")
        val b = java.lang.Double.parseDouble(b_text.getValue + "")

        var cnt = 0
        images.foreach(i => {
          if (i.active) {
            if (selection == null) {
              i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.max , (r, g, b)))
            } else {
              val temp_rect = new Rectangle(0, 0, i.image.getWidth, i.image.getHeight)
              val changed_pixels = new mutable.HashMap[String, Pixel]()
              selection.rectangles.foreach(ri => {
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

                  val rect_changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer[Pixel]()
                  for(ind_i <- y until y + height.toInt)
                    for(ind_j <- x until x + width.toInt) {
                      val p = i.pixels(ind_i * i.image.getWidth + ind_j)
                      if (!changed_pixels.contains(p.x + "-" + p.y)) {
                        p.op_sequence += new MethodInfo(p.max , (r, g, b))
                        rect_changed_pixels += p
                        changed_pixels.addOne(p.x + "-" + p.y, p.clone())
                      }
                    }
                  ri.changed_pixels(cnt) = rect_changed_pixels
                }
              })
            }
          }
          cnt += 1
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

        selection.rectangles.foreach(r => {
          r.changed_pixels.foreach(cp => {
            cp.foreach(p => p.execute_methods())
          })
        })

        dispose()
      }
    })

    this.add(op_panel)
    this.add(rgb_panel)
    this.add(finish)
  }

    selection.rectangles.foreach(r => {
      for (i <- 0 until images.length)
        r.changed_pixels += new ArrayBuffer[Pixel]()
    })

  init()
}
