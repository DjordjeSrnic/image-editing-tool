package misc

import java.awt.Polygon
import scala.collection.mutable.ListBuffer

class SelectionInfo(val name: String, val polygons: ListBuffer[Polygon], val rectangles: ListBuffer[RectangleInfo], var active: Boolean, var selected: Boolean) {

}
