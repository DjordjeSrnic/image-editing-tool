package gui

import image.{MethodInfo, Pixel}
import misc.ImageInfo

import java.awt.GridLayout
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{JButton, JDialog, JFrame, JPanel, JTextField}
import scala.collection.mutable.ListBuffer

class PixelCalculatorDialog(var images: ListBuffer[ImageInfo], owner: JFrame) extends JDialog(owner, true) {

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
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.+ , (r, g, b)))
        })
      }
    })

    sub.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)

        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.- , (r, g, b)))
        })
      }
    })

    inv_sub.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.:- , (r, g, b)))
        })
      }
    })

    mul.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.* , (r, g, b)))
        })
      }
    })

    div.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p./ , (r, g, b)))
        })
      }
    })

    inv_div.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.:/ , (r, g, b)))
        })
      }
    })

    power.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.power , (r, g, b)))
        })
      }
    })

    log.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.log , (r, g, b)))
        })
      }
    })

    abs.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.abs , (r, g, b)))
        })
      }
    })

    min.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.min , (r, g, b)))
        })
      }
    })

    max.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        val r = java.lang.Double.parseDouble(r_text.getText)
        val g = java.lang.Double.parseDouble(g_text.getText)
        val b = java.lang.Double.parseDouble(b_text.getText)
        images.foreach(i => {
          i.pixels.foreach(p => p.op_sequence += new MethodInfo(p.max , (r, g, b)))
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
          i.pixels.foreach(p => p.execute_methods())
          i.update_image()
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
