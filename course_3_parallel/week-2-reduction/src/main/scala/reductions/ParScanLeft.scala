package reductions

/**
  * Created by viktor on 25/03/2017.
  *
  */
object ParScanLeft {
  def scanLeft[A](in: Array[A], a0: A,
                  f: (A, A) => A, out: Array[A]): Unit = {
    out(0) = a0
    var a = a0
    var i = 0
    while (i < in.length) {
      a = f(a, in(i))
      i += 1
      out(i) = a
    }
  }


}
