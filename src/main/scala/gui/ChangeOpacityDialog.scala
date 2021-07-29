package gui

import java.awt._
import java.awt.event._
import javax.swing._

class ChangeOpacityDialog(owner: JFrame) extends JDialog(owner, true) {

  var opacity: Double = -1.0

  private def init(): Unit = {
    setTitle("Change Opacity")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 500, 150)
    setLayout(new GridLayout(2, 1))
    val set_opacity = new JButton("Set Opacity")
    val spinner: JSpinner = new JSpinner(new SpinnerNumberModel(0.5, 0,1,0.1))

    set_opacity.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setAlwaysOnTop(false)
        opacity = spinner.getValue.toString.toDouble
        setAlwaysOnTop(true)
        dispose()
      }
    })


    add(set_opacity)
    add(spinner)
  }

  init()
}
