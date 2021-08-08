package gui

import java.awt.GridLayout
import java.awt.event.{ActionEvent, ActionListener}
import java.util.Scanner
import javax.swing.{JButton, JDialog, JFileChooser, JFrame, JTextField}
import scala.collection.mutable.ListBuffer

class SelectMatrixDialog(owner: JFrame, val N: Int) extends JDialog(owner, true) {
  private val fc: JFileChooser = new JFileChooser()
  val matrix: ListBuffer[Double] = new ListBuffer[Double]()

  private def init(): Unit = {
    setTitle("Select Matrix")
    setAlwaysOnTop(true)
    setVisible(false)
    setBounds(500, 400, 200, 200)
    setLayout(new GridLayout(2, 1))
    val choose = new JButton("Choose")
    val confirm = new JButton("Finish")

    choose.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        setAlwaysOnTop(false)
        val returnVal = fc.showOpenDialog(null)
        if (returnVal == JFileChooser.APPROVE_OPTION) {
          val file = fc.getSelectedFile

          val scanner = new Scanner(file)
          var i = 0
          while (i<(2*N+1)) {
            val numbers = scanner.nextLine.split(" ")
            var j = 0
            while (j<(2*N+1)) {
              matrix += java.lang.Double.parseDouble(numbers(j))
              println(numbers(j))
              j += 1
            }
            i += 1
          }
        }
        setAlwaysOnTop(true)
      }
    })

    confirm.addActionListener(new ActionListener {
      override def actionPerformed(e: ActionEvent): Unit = {
        println(N)
        if (matrix.length == (2*N+1)*(2*N+1)) {
          dispose()
        }
      }
    })

    add(choose)
    add(confirm)
  }

  init()
}
