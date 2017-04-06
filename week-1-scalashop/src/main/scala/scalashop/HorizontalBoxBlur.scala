package scalashop

import common._
import org.scalameter._

object HorizontalBoxBlurRunner {

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
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }

    val seqtimeWhile = standardConfig measure {
      HorizontalWhileBoxBlur.blur(src, dst, 0, height, radius)
    }

    val numTasks = 32
    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }

    val partimeWhile = standardConfig measure {
      HorizontalWhileBoxBlur.parBlur(src, dst, numTasks, radius)
    }

    println(s"sequential blur time: $seqtime ms")
    println(s"sequential blur time with while: $seqtimeWhile ms")
    println(s"Speed Up between for and while: ${seqtime / seqtimeWhile}")

    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")

    println(s"fork/join blur time (while): $partimeWhile ms")
    println(s"speedup (while): ${seqtimeWhile / partimeWhile}")
  }
}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends BoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      y <- from until end
      x <- 0 until src.width
    } dst(x, y) = boxBlurKernel(src, x, y, radius)
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val batch = src.height / numTasks + (if (src.height % numTasks > 0) 1 else 0)

    val tasks = for {i <- 0 until src.height by batch} yield task {
      blur(src, dst, i, clamp(i + batch, 0, src.height), radius)
    }
    tasks.foreach(_.join())
  }

}

object HorizontalWhileBoxBlur extends BoxBlur {

  override def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var x = 0
    var y = from
    while (y < end) {
      x = 0
      while (x < src.width) {
        dst(x, y) = boxBlurKernelWhile(src, x, y, radius)
        x += 1
      }
      y += 1
    }
  }

  override def parBlur(src: Img, dst: Img, numTasks: RGBA, radius: RGBA): Unit = {
    val batch = src.height / numTasks + (if (src.height % numTasks > 0) 1 else 0)

    val tasks = for {i <- 0 until src.height by batch} yield task {
      blur(src, dst, i, clamp(i + batch, 0, src.height), radius)
    }
    tasks.foreach(_.join())
  }
}