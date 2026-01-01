package se.randomserver.ne.the_game

object Utils {
  def splitEvenly[A](vec: Vector[A], parts: Int): Vector[Vector[A]] = {
    require(parts > 0, "parts must be > 0")

    val total = vec.length
    val base  = total / parts
    val extra = total % parts

    var offset = 0

    Vector.tabulate(parts) { i =>
      val size = base + (if (i < extra) 1 else 0)
      val chunk = vec.slice(offset, offset + size)
      offset += size
      chunk
    }
  }
}
