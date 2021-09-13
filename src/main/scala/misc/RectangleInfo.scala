package misc

import scala.collection.mutable.ArrayBuffer

class RectangleInfo(var orig_x: Int, var orig_y: Int, var dest_x: Int, var dest_y: Int, var color: Int = 0) {
  var changed_pixels: ArrayBuffer[ArrayBuffer[Pixel]] = new ArrayBuffer()
  val wrapped_pixels: ArrayBuffer[PixelWrapper] = new ArrayBuffer()
}
