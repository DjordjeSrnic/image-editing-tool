package misc

import scala.collection.mutable.ArrayBuffer

class RectangleInfo(var orig_x: Int, var orig_y: Int, var dest_x: Int, var dest_y: Int, var color: Int = 0) {
  val changed_pixels: ArrayBuffer[Pixel] = new ArrayBuffer()
}
