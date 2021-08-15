package image

import image.Pixel.max_pixel_value

import java.awt.Color
import scala.collection.GenSeq
import scala.collection.mutable.ListBuffer
import scala.math._

class Pixel(val x: Int, val y: Int, var R: Double, var G: Double, var B: Double, var A: Double) {

  val op_sequence: ListBuffer[MethodInfo] = new ListBuffer[MethodInfo]()
  var color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB

  def + (const: (Double, Double, Double)) = const match {
    case (d_r, d_g, d_b) => {
      this.R = this.R + d_r
      this.G = this.G + d_g
      this.B = this.B + d_b
    }
  }

  def - (const: (Double, Double, Double)) = const match {
    case (d_r, d_g, d_b) => {
      this.R = this.R - d_r
      this.G = this.G - d_g
      this.B = this.B - d_b
    }
  }

  def :- (const: (Double, Double, Double)) = const match {
    case (d_r, d_g, d_b) => {
      this.R = d_r - this.R
      this.G = d_g - this.G
      this.B = d_b - this.B
    }
  }

  def * (const: (Double, Double, Double)) = const match {
    case (d_r, d_g, d_b) => {
      this.R = this.R * d_r
      this.G = this.G * d_g
      this.B = this.B * d_b
    }
  }

  def / (const: (Double, Double, Double)) = const match {
    case (d_r, d_g, d_b) => {
      this.R = this.R / d_r
      this.G = this.G / d_g
      this.B = this.B / d_b
    }
  }

  def :/ (const: (Double, Double, Double)) = const match {
    case (d_r, d_g, d_b) => {
      this.R = d_r / this.R
      this.G = d_g / this.G
      this.B = d_b / this.B
    }
  }

  def power (const: (Double, Double, Double)) = const match {
    case (p_r, p_g, p_b) => {
      this.R = scala.math.pow(this.R, p_r)
      this.G = scala.math.pow(this.G, p_g)
      this.B = scala.math.pow(this.B, p_b)
    }
  }

  def log (const: (Double, Double, Double) = (1.0, 1.0, 1.0)) = {
    val c: Double = 1.0 / scala.math.log(1/255 + max_pixel_value)

    this.R = c * scala.math.log(1/255 + this.R)
    this.G = c * scala.math.log(1/255 + this.G)
    this.B = c * scala.math.log(1/255 + this.B)
  }

  def abs (const: (Double, Double, Double)) = const match {
    case (p_r, p_g, p_b) => {
      this.R = scala.math.abs(this.R - p_r)
      this.G = scala.math.abs(this.G - p_g)
      this.B = scala.math.abs(this.B - p_b)
    }
  }

  def min (const: (Double, Double, Double)) = const match {
    case (r, g, b) => {
      val A: Int = new Color((this.R*255).toInt, (this.G*255).toInt, (this.B*255).toInt, (this.A*255).toInt).getRGB
      val B: Int = new Color((r*255).toInt, (g*255).toInt, (b*255).toInt, (this.A*255).toInt).getRGB
      if (A > B) {
        this.R = r
        this.G = g
        this.B = b
      }
    }
  }

  def max (const: (Double, Double, Double)) = const match {
    case (r, g, b) => {
      val A: Int = new Color((this.R*255).toInt, (this.G*255).toInt, (this.B*255).toInt, (this.A*255).toInt).getRGB
      val B: Int = new Color((r*255).toInt, (g*255).toInt, (b*255).toInt, (this.A*255).toInt).getRGB
      if (A < B) {
        this.R = r
        this.G = g
        this.B = b
      }
    }
  }

  def set_opacity(A: Double) = {
    this.A = A
    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }

  /*def composite(x: (Double, Double, Double)): Pixel = {
    val ret = op_sequence.foldLeft(x)((acc, curr) => curr(acc._1, acc._2, acc._3))
  }*/

  def negative() = {
    this.R = 1 - this.R
    this.G = 1 - this.G
    this.B = 1 - this.B
    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }

  def grayscale() = {
    val mean: Double = (this.R + this.G + this.B) / 3
    this.R = mean
    this.G = mean
    this.B = mean
    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }

  def median_filter(neighbors: Array[Pixel], N: Int) = {
    val list: ListBuffer[Pixel] = ListBuffer()

    neighbors.foreach(p => {
      if (scala.math.abs(p.x - x) <= N && scala.math.abs(p.y - y) <= N)
        list += p
    })

    val lR = list.sortBy(p => p.R)
    val lG = list.sortBy(p => p.G)
    val lB = list.sortBy(p => p.B)
    val len: Int = list.length

    val median = if (len % 2 == 0) {
      val r: Double = (lR(len/2).R + lR(len/2 - 1).R) / 2
      val g: Double = (lG(len/2).G + lG(len/2 - 1).G) / 2
      val b: Double = (lB(len/2).B + lB(len/2 - 1).B) / 2

      (r, g, b)
    } else {
      val r: Double = lR((len-1)/2).R
      val g: Double = lG((len-1)/2).G
      val b: Double = lB((len-1)/2).B

      (r, g, b)
    }

    this.R = median._1
    this.G = median._2
    this.B = median._3
    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }

  def weighted_filter(neighbors: Array[Pixel], weight_matrix: Array[Double], N: Int): Unit = {
    var cnt = 0
    var sumR: Double = 0
    var sumG: Double = 0
    var sumB: Double = 0

    neighbors.foreach(p => {
      if (scala.math.abs(p.x - x) <= N && scala.math.abs(p.y - y) <= N) {
        sumR += weight_matrix(cnt) * p.R
        sumG += weight_matrix(cnt) * p.G
        sumB += weight_matrix(cnt) * p.B
        cnt += 1
      }
    })

    this.R = sumR / cnt
    this.G = sumG / cnt
    this.B = sumB / cnt
    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }

  def execute_methods() = {
    op_sequence.foreach(o => {
      o.func(o.args._1, o.args._2, o.args._3)
    })

    if (R < 0 || R > 1.0)
       R = 1.0

    if (G < 0 || G > 1.0)
       G = 1.0

    if (B < 0 || B > 1.0)
       B = 1.0

    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }
}

object Pixel {
  var max_pixel_value: Double = 0
}