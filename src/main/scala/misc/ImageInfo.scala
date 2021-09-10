package misc

import java.awt.image.BufferedImage
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class ImageInfo(val name: String, var image: BufferedImage, val layer: Int, var opacity: Double = 1.0, var active: Boolean = true) {
  var pixels: ArrayBuffer[Pixel] = new ArrayBuffer()
  var orig_pixels: ArrayBuffer[Pixel] = new ArrayBuffer()

  def update_image() = {
    image.setRGB(0, 0, image.getWidth, image.getHeight, pixels.map(_.color_value).toArray, 0, image.getWidth)
  }

  def init() = {
    val rgb = image.getRGB(0, 0, image.getWidth, image.getHeight, null, 0, image.getWidth)

    for(y <- 0 until image.getHeight)
      for(x <- 0 until image.getWidth) {
        val c_hex = rgb(y*image.getWidth + x).toHexString.split("")
        val r:Double = java.lang.Short.parseShort(c_hex(2) + c_hex(3), 16)/255.0
        val g:Double = java.lang.Short.parseShort(c_hex(4) + c_hex(5), 16)/255.0
        val b:Double = java.lang.Short.parseShort(c_hex(6) + c_hex(7), 16)/255.0
        val a:Double = java.lang.Short.parseShort(c_hex(0) + c_hex(1), 16)/255.0

        pixels += new Pixel(x, y, r, g, b, a)
        orig_pixels += new Pixel(x, y, r, g, b, a)
      }
  }

  init()
}
