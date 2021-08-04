package image

import java.awt.Color
import scala.collection.mutable.ListBuffer
import scala.math._

class Pixel(val x: Int, val y: Int, var R: Double, var G: Double, var B: Double, var A: Double) {

  var color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB

  def + (const: (Double, Double, Double)): Pixel = const match {
    case (d_r, d_g, d_b) =>
      new Pixel(this.x, this.y, this.R + d_r, this.G + d_g, this.B + d_b, this.A)
  }

  def - (const: (Double, Double, Double)): Pixel = const match {
    case (d_r, d_g, d_b) =>
      new Pixel(this.x, this.y, this.R - d_r, this.G - d_g, this.B - d_b, this.A)
  }

  def :- (const: (Double, Double, Double)): Pixel = const match {
    case (d_r, d_g, d_b) =>
      new Pixel(this.x, this.y, d_r - this.R, d_g - this.G, d_b - this.B, this.A)
  }

  def * (const: (Double, Double, Double)): Pixel = const match {
    case (d_r, d_g, d_b) =>
      new Pixel(this.x, this.y, this.R * d_r, this.G * d_g, this.B * d_b, this.A)
  }

  def / (const: (Double, Double, Double)): Pixel = const match {
    case (d_r, d_g, d_b) =>
      new Pixel(this.x, this.y, this.R / d_r, this.G / d_g, this.B / d_b, this.A)
  }

  def power (const: (Double, Double, Double)): Pixel = const match {
    case (p_r, p_g, p_b) =>
      new Pixel(this.x, this.y, scala.math.pow(this.R, p_r), scala.math.pow(this.G, p_g), scala.math.pow(this.B, p_b), this.A)
  }

  def log: Pixel = {
    new Pixel(this.x, this.y, scala.math.log(this.R), scala.math.log(this.G), scala.math.log(this.B), this.A)
  }

  def abs: Pixel = {
    new Pixel(this.x, this.y, scala.math.abs(this.R), scala.math.abs(this.G), scala.math.abs(this.B), this.A)
  }

  def min (const: (Double, Double, Double)): Pixel = const match {
    case (r, g, b) => {
      val A: Double = 0.299*this.R + 0.587*this.G + 0.114*this.B
      val B: Double = 0.299*r + 0.587*r + 0.114*b
      if (A > B) new Pixel(this.x, this.y, r, g, b, this.A)
      else new Pixel(this.x, this.y, this.R, this.G, this.B, this.A)
    }
  }

  def max (const: (Double, Double, Double)): Pixel = const match {
    case (r, g, b) => {
      val A: Double = 0.299*this.R + 0.587*this.G + 0.114*this.B
      val B: Double = 0.299*r + 0.587*g + 0.114*b
      if (A < B) new Pixel(this.x, this.y, r, g, b, this.A)
      else new Pixel(this.x, this.y, this.R, this.G, this.B, this.A)
    }
  }

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
      if (p.x != x || p.y != y) {
        val dist: Double = scala.math.sqrt(scala.math.pow(x-p.x, 2) + scala.math.pow(y-p.y, 2))
        if (dist <= N)
          list += p
      }
    })

    list.sortBy(p => p.color_value)
    val len: Int = list.length

    val median = if (len % 2 == 0) {
      val r: Double = (list(len/2).R + list(len/2 - 1).R) / 2
      val g: Double = (list(len/2).G + list(len/2 - 1).G) / 2
      val b: Double = (list(len/2).B + list(len/2 - 1).B) / 2

      (r, g, b)
    } else {
      val r: Double = list((len-1)/2).R
      val g: Double = list((len-1)/2).G
      val b: Double = list((len-1)/2).B

      (r, g, b)
    }

    this.R = median._1
    this.G = median._2
    this.B = median._3
    color_value = new Color((R*255).toInt, (G*255).toInt, (B*255).toInt, (A*255).toInt).getRGB
  }

  def weighted_filter(neighbors: Array[Array[Pixel]], weight_matrix: Array[Array[Double]], N: Int): Double = {
    val start_row: Int = if (x <= N) 0 else x - N
    val end_row: Int = if (x + N >= neighbors.length) neighbors.length - 1 else x + N
    val start_col: Int = if (y <= N) 0 else y - N
    val end_col: Int = if (y + N >= neighbors(0).length) neighbors(0).length - 1 else y + N

    val list: ListBuffer[Pixel] = ListBuffer()
    var sum: Double = 0
    var cnt: Int = 0
    for (i <- start_row to end_row)
      for (j <- start_col to end_col) {
        val dist: Double = scala.math.sqrt(scala.math.pow(x-neighbors(i)(j).x, 2) + scala.math.pow(y-neighbors(i)(j).y, 2))
        if (dist <= N) {
          val r: Double = neighbors(i)(j).R
          val g: Double = neighbors(i)(j).G
          val b: Double = neighbors(i)(j).B
          sum = sum + weight_matrix(i)(j) * (0.299*r + 0.587*r + 0.114*r)
          cnt = cnt + 1
        }
      }

    sum = sum/cnt
    sum
  }
}
