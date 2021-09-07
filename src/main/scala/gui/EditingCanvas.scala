package gui

import misc.{ImageInfo, RectangleInfo, SelectionInfo}

import java.awt._
import java.awt.event.{MouseAdapter, MouseEvent, MouseMotionAdapter}
import javax.swing.JPanel
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class EditingCanvas(var image_list: ListBuffer[ImageInfo]) extends JPanel {
  override def getPreferredSize = new Dimension(400, 200)

  var curr_selection: SelectionInfo = null
  var rectangle_list: ListBuffer[RectangleInfo] = ListBuffer()
  var orig_x = -1
  var orig_y = -1
  var dest_x = -1
  var dest_y = -1
  var changed = false
  var cnt = 0


  private def getOverlapingRectangles(rectangles: ListBuffer[RectangleInfo]): ListBuffer[ListBuffer[RectangleInfo]] = {
    var ret: ListBuffer[ListBuffer[RectangleInfo]] = ListBuffer()
    var rects: ListBuffer[RectangleInfo] = ListBuffer()
    rectangles.reverse.foreach(r => rects += r)

    while(!rects.isEmpty) {
      var group: ListBuffer[RectangleInfo] = ListBuffer()
      var queue: ListBuffer[RectangleInfo] = ListBuffer()
      queue += rects.remove(0)
      while(!queue.isEmpty) {
        val rect_0 = queue.remove(0)
        val rect_iter = rects.iterator
        while (rect_iter.hasNext) {
          val rect_1 = rect_iter.next()
          val r0: Rectangle = new Rectangle(rect_0.orig_x, rect_0.orig_y, rect_0.dest_x - rect_0.orig_x, rect_0.dest_y - rect_0.orig_y)
          val r1: Rectangle = new Rectangle(rect_1.orig_x, rect_1.orig_y, rect_1.dest_x - rect_1.orig_x, rect_1.dest_y - rect_1.orig_y)
          if (r0.intersects(r1)) {
            queue += rect_1
            rects = rects.filter(r => r != rect_1)
          }
        }
        group += rect_0
      }
      ret += group
    }

    ret
  }

  private def makePolygon(rectangles: ListBuffer[RectangleInfo]): Polygon = {
    val points: ListBuffer[Point] = calculatePoints(rectangles)
    var polygon = new Polygon()
    points.foreach(p => {
      polygon.addPoint(p.x, p.y)
    })
    polygon
  }

  private def getAllYCoords(rectangles: ListBuffer[RectangleInfo]): mutable.Set[Float] = {
    val allTopYCoords: ListBuffer[Float] = rectangles.map(r => r.orig_y)
    val allBottomYCoords: ListBuffer[Float] = rectangles.map(r => r.dest_y)

    val allCoords: mutable.Set[Float] = mutable.HashSet()
    allTopYCoords.foreach(t => allCoords.add(t))
    allBottomYCoords.foreach(b => allCoords.add(b))
    allCoords
  }

  private def rectsAtYExcBottomLines(y: Float, rectangles: ListBuffer[RectangleInfo]): ListBuffer[RectangleInfo] = {
    rectangles.filter(r => r.orig_y <= y && r.dest_y > y)
  }

  private def rectsAtYIncBottomLines(y: Float, rectangles: ListBuffer[RectangleInfo]): ListBuffer[RectangleInfo] = {
    rectangles.filter(r => r.orig_y <= y && r.dest_y == y)
  }

  private def rectanglesAtY(y: Float, rectangles: ListBuffer[RectangleInfo]): ListBuffer[RectangleInfo] = {
    val rectsAtYExcBottomLines: ListBuffer[RectangleInfo] = this.rectsAtYExcBottomLines(y, rectangles)

    if (rectsAtYExcBottomLines.length > 0) {
      // there are rectangles that are not closing here, so ignore those that are closing.
      rectsAtYExcBottomLines
    }
    else {
      // there are only rectangle bottom lines so we need to consider them.
      this.rectsAtYIncBottomLines(y, rectangles)
    }
  }

  def minXLeftCoord(y: Float, rectangles: ListBuffer[RectangleInfo]): Float = {
    rectanglesAtY(y, rectangles).map(r => r.orig_x).sorted.head
  }

  def maxXRightCoord(y: Float, rectangles: ListBuffer[RectangleInfo]): Float = {
    rectanglesAtY(y, rectangles).map(r => r.dest_x).sorted.last
  }

  private def calculatePoints(rectangles: ListBuffer[RectangleInfo]): ListBuffer[Point] = {
    var ret: ListBuffer[Point] = ListBuffer()

    var yCoords: ListBuffer[Float] = ListBuffer()
    getAllYCoords(rectangles).toList.foreach(y => yCoords += y)
    yCoords = yCoords.sorted

    var previousLeftCoord: Float = 0
    var previousRightCoord: Float = 0

    yCoords.foreach(y => {
      println("Considering yCoords " + y)
      val minimumXLeftCoord = minXLeftCoord(y, rectangles)
      val maximumXRightCoord = maxXRightCoord(y, rectangles)
      println("min X: " + minimumXLeftCoord)
      println("max X: " + maximumXRightCoord)

      if (y == yCoords.head) {
        ret += new Point(minimumXLeftCoord.toInt, y.toInt)
        ret += new Point(maximumXRightCoord.toInt, y.toInt)
      } else {
        if (minimumXLeftCoord != previousLeftCoord) {
          ret = ret.prepend(new Point(previousLeftCoord.toInt, y.toInt))
          ret = ret.prepend(new Point(minimumXLeftCoord.toInt, y.toInt))
        }
        else
          ret = ret.prepend(new Point(minimumXLeftCoord.toInt, y.toInt))

        if (maximumXRightCoord != previousRightCoord) {
          ret += new Point(previousRightCoord.toInt, y.toInt)
          ret += new Point(maximumXRightCoord.toInt, y.toInt)
        }
        else
          ret += new Point(maximumXRightCoord.toInt, y.toInt)
      }

      previousLeftCoord = minimumXLeftCoord
      previousRightCoord = maximumXRightCoord
    })

    ret
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)

    var g2d: Graphics2D = g.asInstanceOf[Graphics2D]
    if (changed == true) {
      image_list.foreach(i => {
        if (i.active) {
          val alpha = i.opacity.toFloat
          g2d.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha))
          g2d.drawImage(i.image, 0, 0, null)
        }
      })
      changed = false
    }

    //val rect_groups: ListBuffer[ListBuffer[RectangleInfo]] = getOverlapingRectangles(rectangle_list)

    //    rect_groups.foreach(rg => {
    //      val polygon: Polygon = makePolygon(rg)
    //      g.setColor(Color.RED)
    //      g.fillPolygon(polygon)
    //    })
  }

  def update_list(new_list: ListBuffer[ImageInfo]) = {
    this.image_list = new_list
  }

  def init(): Unit = {
    this.addMouseListener(new MouseAdapter {
      override def mousePressed(e: MouseEvent): Unit = {
        orig_x = e.getX
        orig_y = e.getY
      }

      override def mouseReleased(e: MouseEvent): Unit = {
        rectangle_list += new RectangleInfo(orig_x, orig_y, dest_x, dest_y)
        changed = true
        repaint()
      }
    })

    this.addMouseMotionListener(new MouseMotionAdapter {
      override def mouseDragged(e: MouseEvent): Unit = {
        dest_x = e.getX
        dest_y = e.getY

      }
    })
  }

  init()
}