package misc

import java.awt.Color
import scala.collection.mutable.ListBuffer

class PixelWrapper(val pixel: Pixel) {

  var comp_sequence: ListBuffer[((Double, Double, Double)) => (Double, Double, Double)] = new ListBuffer()

  def + (const: (Double, Double, Double)) =  {
    pixel + const
    (pixel.R, pixel.G, pixel.B)
  }

  def - (const: (Double, Double, Double)) = {
    pixel - const
    (pixel.R, pixel.G, pixel.B)
  }

  def :- (const: (Double, Double, Double)) = {
    pixel :- const
    (pixel.R, pixel.G, pixel.B)
  }

  def * (const: (Double, Double, Double)) = {
    pixel * const
    (pixel.R, pixel.G, pixel.B)
  }

  def / (const: (Double, Double, Double)) = {
    pixel / const
    (pixel.R, pixel.G, pixel.B)
  }

  def :/ (const: (Double, Double, Double)) = {
    pixel :/ const
    (pixel.R, pixel.G, pixel.B)
  }

  def power (const: (Double, Double, Double)) = {
    pixel power const
    (pixel.R, pixel.G, pixel.B)
  }

  def log (const: (Double, Double, Double) = (1.0, 1.0, 1.0)) = {
    pixel log const
    (pixel.R, pixel.G, pixel.B)
  }

  def abs (const: (Double, Double, Double)) = {
    pixel abs const
    (pixel.R, pixel.G, pixel.B)
  }

  def min (const: (Double, Double, Double)) = {
    pixel min const
    (pixel.R, pixel.G, pixel.B)
  }

  def max (const: (Double, Double, Double)) = {
    pixel max const
    (pixel.R, pixel.G, pixel.B)
  }

  def composite(const: (Double, Double, Double)) = {
    comp_sequence.foldLeft(const) {
      (args, op) => {
        op(args)
      }
    }
    if (pixel.R < 0 || pixel.R > 1.0)
      pixel.R = 1.0

    if (pixel.G < 0 || pixel.G > 1.0)
      pixel.G = 1.0

    if (pixel.B < 0 || pixel.B > 1.0)
      pixel.B = 1.0

    pixel.color_value = new Color((pixel.R*255).toInt, (pixel.G*255).toInt, (pixel.B*255).toInt, (pixel.A*255).toInt).getRGB
  }
}

object PixelWrapper {
  var cnt = 0
}
