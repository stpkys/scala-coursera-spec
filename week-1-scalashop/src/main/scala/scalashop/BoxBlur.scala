package scalashop

/**
  * Created by viktor on 06/04/2017.
  *
  */
trait BoxBlur {
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit
}
