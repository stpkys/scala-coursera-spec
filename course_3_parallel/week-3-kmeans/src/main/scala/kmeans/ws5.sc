import scala.collection.GenSeq

val k: GenSeq[Int] = Seq(1, 2, 3)

k.map(a => a -> GenSeq()).toMap


