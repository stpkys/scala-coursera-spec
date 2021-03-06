package scalashop

import java.util.concurrent.ForkJoinTask

import common._
import org.scalameter._


object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur extends BoxBlur {

  /** Blurs the columns of the source image `src` into the destination image
    * `dst`, starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each column, `blur` traverses the pixels by going from top to
    * bottom.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit =
    for {
      x <- from until end
      y <- 0 until src.height
    } dst(x, y) = boxBlurKernel(src, x, y, radius)

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * columns.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val batch = src.width / numTasks + (if ((src.width % numTasks) > 0) 1 else 0)

    (for (i <- 0 until src.width by batch) yield task {
      blur(src, dst, i, clamp(i + batch, 0, src.width), radius)
    }).foreach(_.join())
  }
}

object VerticalWhileBoxBlur extends BoxBlur {

  override def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var x = from
    var y = 0

    while (x < end) {
      y = 0
      while (y < src.height) {
        dst(x, y) = boxBlurKernelWhile(src, x, y, radius)
        y += 1
      }
      x += 1
    }
  }

  override def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val batch = src.width / numTasks + (if ((src.width % numTasks) > 0) 1 else 0)

    val tasks: Array[ForkJoinTask[Unit]] = new Array[ForkJoinTask[Unit]](numTasks)

    var i = 0
    while (i < numTasks) {
      val j = i
      tasks(i) = task {
        blur(src, dst, j * batch, clamp((j + 1) * batch, 0, src.width), radius)
      }
      i += 1
    }

    tasks.foreach(_.join())
  }
}