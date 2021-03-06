import scala.annotation.tailrec

package object scalashop {

  /** The value of every pixel is represented as a 32 bit integer. */
  type RGBA = Int

  /** Returns the red component. */
  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  /** Returns the green component. */
  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  /** Returns the blue component. */
  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  /** Returns the alpha component. */
  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  /** Used to create an RGBA value from separate components. */
  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  /** Restricts the integer into the specified range. */
  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  /** Image is a two-dimensional matrix of pixel values. */
  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  /** Computes the blurred RGBA value of a single pixel of the input image. */
  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if (radius == 0) return src(x, y)
    var r = 0
    var g = 0
    var b = 0
    var a = 0
    var count = 0

    def clamp2(min: Int, max: Int)(v: Int) = clamp(v, min, max)

    val boundWidth = clamp2(0, src.width - 1)(_)
    val boundHeight = clamp2(0, src.height - 1)(_)

    for {
      dy <- boundHeight(y - radius) to boundHeight(y + radius)
      dx <- boundWidth(x - radius) to boundWidth(x + radius)
    } {
        val pix = src(dx, dy)
        r += red(pix)
        g += green(pix)
        b += blue(pix)
        a += alpha(pix)
        count += 1
    }

    rgba(r / count, g / count, b / count, a / count)
  }

  def boxBlurKernelWhile(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    if (radius == 0) return src(x, y)
    var r = 0
    var g = 0
    var b = 0
    var a = 0
    var count = 0

    val dy = Math.max(0, y - radius)
    val ly = Math.min(y + radius, src.height - 1)

    val dx = Math.max(0, x - radius)
    val lx = Math.min(x + radius, src.width - 1)

    var cx = 0
    var cy = dy

    while (cy <= ly) {
      cx = dx
      while (cx <= lx) {
        val pix = src(cx, cy)
        r += red(pix)
        g += green(pix)
        b += blue(pix)
        a += alpha(pix)
        count += 1
        cx += 1
      }
      cy += 1
    }

    rgba(r / count, g / count, b / count, a / count)
  }

  def boxBlurKernelRecursion(src: Img, x: Int, y: Int, radius: Int): RGBA = {
    val startX = clamp(x - radius, 0, src.width)
    val startY = clamp(y - radius, 0, src.height)
    val dx = Math.min(x + radius, src.width - 1) - startX
    val dy = Math.min(y + radius, src.height - 1) - startY
    val d = (dx + 1) * (dy + 1)

    @tailrec
    def loop(r: Int, g: Int, b: Int, a: Int, x: Int, y: Int): RGBA = {
      if (y > dy) rgba(r / d, g / d, b / d, a / d)
      else {
        val pix = src(startX + x, startY + y) //startX + count % dx, startY + count / dx)
        loop(
          r + red(pix), g + green(pix), b + blue(pix), a + alpha(pix),
          if (x < dx) x + 1 else 0,
          if (x < dx) y else y + 1
        )
      }
    }

    loop(0, 0, 0, 0, 0, 0)
  }
}
