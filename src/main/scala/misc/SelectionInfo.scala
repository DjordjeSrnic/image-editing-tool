package misc

import java.awt.Polygon
import scala.collection.mutable.ListBuffer

class SelectionInfo(val name: String, val polygons: ListBuffer[Polygon], val rectangles: ListBuffer[RectangleInfo], var active: Boolean) {
  val previous_state: ListBuffer[ImageInfo] = new ListBuffer[ImageInfo]()
  val new_state: ListBuffer[ImageInfo] = new ListBuffer[ImageInfo]()
}
