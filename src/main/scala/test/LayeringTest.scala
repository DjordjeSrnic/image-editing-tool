package test

import javax.swing._
import java.awt._

object LayeringTest extends App {
  var color_value = Color.BLUE
  println(color_value.getTransparency)
  color_value = new Color(color_value.getRed, color_value.getBlue, color_value.getGreen, 127)
  println(color_value.getTransparency)
}
