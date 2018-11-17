package scalashop

import common._
import org.scalameter._

import scalashop.HorizontalBoxBlur.blur

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
    val numTasks = 8

    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }

    val seqtimeWhile = standardConfig measure {
      HorizontalWhileBoxBlur.blur(src, dst, 0, height, radius)
    }

    val boxBlur = new HorizontalWhileBoxBlur(new RecursionBlurKernel)

    val seqTimeWhileAndRec = standardConfig measure {
      boxBlur.blur(src, dst, 0, height, radius)
    }

    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }

    val partimeWhile = standardConfig measure {
      HorizontalWhileBoxBlur.parBlur(src, dst, numTasks, radius)
    }

    println(s"sequential blur time: $seqtime ms")
    println(s"sequential blur time with while: $seqtimeWhile ms")
    println(s"sequential blur time with while and rec: $seqTimeWhileAndRec ms")
    println(s"Speed Up between for and while: ${seqtime / seqtimeWhile}")

    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")

    println(s"fork/join blur time (while): $partimeWhile ms")
    println(s"speedup (while): ${seqtimeWhile / partimeWhile}")
  }
}

trait HorParBlur extends BoxBlur {
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val batch = src.height / numTasks + (if (src.height % numTasks > 0) 1 else 0)

    val tasks = for {i <- 0 until src.height by batch} yield task {
      blur(src, dst, i, clamp(i + batch, 0, src.height), radius)
    }
    tasks.foreach(_.join())
  }
}

class HorizontalBoxBlur(blurKernel: BlurKernel) extends BoxBlur with HorParBlur {
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      y <- from until end
      x <- 0 until src.width
    } dst(x, y) = blurKernel.blur(src, x, y, radius)
  }
}

/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur extends BoxBlur {

  val boxBlur = new HorizontalBoxBlur(new ForBlurKernel)

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    boxBlur.blur(src, dst, from, end, radius)
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    boxBlur.parBlur(src, dst, numTasks, radius)
  }
}

class HorizontalWhileBoxBlur(blurKernel: BlurKernel) extends BoxBlur with HorParBlur {

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var x = 0
    var y = from
    while (y < end) {
      x = 0
      while (x < src.width) {
        dst(x, y) = blurKernel.blur(src, x, y, radius)
        x += 1
      }
      y += 1
    }
  }
}

object HorizontalWhileBoxBlur extends BoxBlur {
  val boxBlur = new HorizontalWhileBoxBlur(new WhileBlurKernel)

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    boxBlur.blur(src, dst, from, end, radius)
  }
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = boxBlur.parBlur(src, dst, numTasks, radius)
}