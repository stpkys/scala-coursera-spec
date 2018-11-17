import reductions._
import reductions.ParScanLeft._

val a = Array[Int](1, 3, 8)
val out = new Array[Int](4)

scanLeft[Int](a, 100, (a, b) => a + b, out)
out

