package scalashop

trait BoxBlur {
  /** Blurs the rows of the source image `src` into the destination image `dst`,
    * starting with `from` and ending with `end` (non-inclusive).
    *
    * Within each row, `blur` traverses the pixels by going from left to right.
    */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
    *
    * Parallelization is done by stripping the source image `src` into
    * `numTasks` separate strips, where each strip is composed of some number of
    * rows.
    */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
}


trait BlurKernel {
  def blur: (Img, Int, Int, Int) => RGBA
}

class ForBlurKernel extends BlurKernel {
  def blur: (Img, Int, Int, Int) => RGBA = scalashop.boxBlurKernel
}
class WhileBlurKernel extends BlurKernel {
  def blur: (Img, Int, Int, Int) => RGBA = scalashop.boxBlurKernelWhile
}

class RecursionBlurKernel extends BlurKernel {
  def blur: (Img, Int, Int, Int) => RGBA = scalashop.boxBlurKernelRecursion
}