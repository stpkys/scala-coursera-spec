package scalashop

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BlurSuite extends FunSuite {

  val blurKernelImpls: List[((Img, RGBA, RGBA, RGBA) => RGBA, String)] = List(
    (boxBlurKernel, "boxBlurKernel"),
    (boxBlurKernelWhile, "boxBlurKernelWhile"),
    (boxBlurKernelRecursion, "boxBlurKernelRecursion")
  )

  blurKernelImpls.foreach({ case (blurKernel, blurName) =>

    test(s"$blurName should correctly handle radius 0") {
      val src = new Img(5, 5)

      for (x <- 0 until 5; y <- 0 until 5)
        src(x, y) = rgba(x, y, x + y, math.abs(x - y))

      for (x <- 0 until 5; y <- 0 until 5)
        assert(blurKernel(src, x, y, 0) === rgba(x, y, x + y, math.abs(x - y)),
          "boxBlurKernel(_,_,0) should be identity.")
    }

    test(s"$blurName should return the correct value on an interior pixel " +
      "of a 3x4 image with radius 1") {
      val src = new Img(3, 4)
      src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
      src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
      src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8
      src(0, 3) = 50; src(1, 3) = 11; src(2, 3) = 16

      assert(blurKernel(src, 1, 2, 1) === 12,
        s"($blurName(1, 2, 1) should be 12, " +
          s"but it's ${blurKernel(src, 1, 2, 1)})")
    }
  })


  val horizontalBlurImpls: List[((Img, Img, RGBA, RGBA, RGBA) => Unit, String)] = List(
    (HorizontalBoxBlur.blur, "HorizontalBoxBlur"),
    (HorizontalWhileBoxBlur.blur, "HorizontalWhileBoxBlur")
  )

  horizontalBlurImpls.foreach({ case (blur, blurName) =>
    test(s"$blurName.blur with radius 1 should correctly blur the entire 3x3 image") {
      val w = 3
      val h = 3
      val src = new Img(w, h)
      val dst = new Img(w, h)
      src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2
      src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5
      src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8

      blur(src, dst, 0, 2, 1)

      def check(x: Int, y: Int, expected: Int) =
        assert(dst(x, y) == expected,
          s"(destination($x, $y) should be $expected)")

      check(0, 0, 2)
      check(1, 0, 2)
      check(2, 0, 3)
      check(0, 1, 3)
      check(1, 1, 4)
      check(2, 1, 4)
      check(0, 2, 0)
      check(1, 2, 0)
      check(2, 2, 0)
    }
  })

  val verticalBlurImpls: List[((Img, Img, RGBA, RGBA, RGBA) => Unit, String)] = List(
    (VerticalBoxBlur.blur, "VerticalBoxBlur"),
    (VerticalWhileBoxBlur.blur, "VerticalWhileBoxBlur")
  )

  verticalBlurImpls.foreach({case (blur, blurName) =>
    test(s"$blurName.blur with radius 2 should correctly blur the entire " +
      "4x3 image") {
      val w = 4
      val h = 3
      val src = new Img(w, h)
      val dst = new Img(w, h)
      src(0, 0) = 0; src(1, 0) = 1; src(2, 0) = 2; src(3, 0) = 9
      src(0, 1) = 3; src(1, 1) = 4; src(2, 1) = 5; src(3, 1) = 10
      src(0, 2) = 6; src(1, 2) = 7; src(2, 2) = 8; src(3, 2) = 11

      VerticalWhileBoxBlur.blur(src, dst, 0, 4, 2)

      def check(x: Int, y: Int, expected: Int) =
        assert(dst(x, y) == expected,
          s"(destination($x, $y) should be $expected)")

      check(0, 0, 4)
      check(1, 0, 5)
      check(2, 0, 5)
      check(3, 0, 6)
      check(0, 1, 4)
      check(1, 1, 5)
      check(2, 1, 5)
      check(3, 1, 6)
      check(0, 2, 4)
      check(1, 2, 5)
      check(2, 2, 5)
      check(3, 2, 6)
    }
  })
}
