package misc

import java.awt.Polygon
import scala.collection.mutable.ListBuffer

class SelectionInfo(val polygons: ListBuffer[Polygon], var visible: Boolean) {
}
