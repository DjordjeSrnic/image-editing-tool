package misc

import java.awt.Polygon
import scala.collection.mutable.ListBuffer

class SelectionInfo(val polygons: ListBuffer[Polygon], val rectangles: ListBuffer[RectangleInfo], var active: Boolean) {
}
