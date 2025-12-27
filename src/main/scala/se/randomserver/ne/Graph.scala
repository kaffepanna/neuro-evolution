package se.randomserver.ne

import se.randomserver.ne.genome.Genome

object Graph {


  def tarjanSCC[V](nodes: Set[V], edges: V => Set[V]): Set[Set[V]] = {
    import scala.collection.mutable
    val index = mutable.Map.empty[V, Int]
    val lowlink = mutable.Map.empty[V, Int]
    val stack = mutable.Stack.empty[V]
    val onStack = mutable.Set.empty[V]
    var currentIndex = 0
    val result = mutable.Buffer.empty[Set[V]]

    def strongConnect(v: V): Unit = {
      index(v) = currentIndex
      lowlink(v) = currentIndex
      currentIndex += 1
      stack.push(v)
      onStack += v

      for (w <- edges(v)) {
        if (!index.contains(w)) {
          strongConnect(w)
          lowlink(v) = lowlink(v).min(lowlink(w))
        } else if (onStack(w)) {
          lowlink(v) = lowlink(v).min(index(w))
        }
      }

      if (lowlink(v) == index(v)) {
        val scc = mutable.Buffer.empty[V]
        var done = false
        while (!done && stack.nonEmpty) {
          val w = stack.pop()
          onStack -= w
          scc += w
          if (w == v) done = true
        }
        result += scc.toSet
      }
    }

    for (v <- nodes if !index.contains(v)) strongConnect(v)

    result.toSet
  }

  def kahnTopoSort[V](
    nodes: Set[V],
    edges: V => Set[V]
  ): Seq[V] = {
    import scala.collection.immutable.Queue
    val indegree0 = nodes.map(_ -> 0).toMap

    val indegree =
      nodes
        .toSeq
        .flatMap(n => edges(n))
        .foldLeft(indegree0) { (m, to) =>
          m.updated(to, m(to) + 1)
        }

    def run(queue: Queue[V], indeg: Map[V, Int], acc: Vector[V]): Seq[V] = {
      queue.dequeueOption match {
        case None => acc
        case Some(n, rest) => 
          val (indeg_, toQueue) = edges(n).toSeq.foldLeft((indeg, Vector.empty[V])) {
            case ((m,q), to) =>
              val degree = m(to) - 1
              val m_ = m.updated(to, degree)
              if (degree == 0) (m_, q :+ to) 
              else (m_, q)
          }

          run(rest.enqueueAll(toQueue), indeg_, acc :+ n)
      }
    }

    val start = Queue.from(indegree.collect { case (n, 0) => n })
    val result = run(start, indegree, Vector.empty)

    if(result.size != nodes.size)
      throw new IllegalArgumentException("Cycle detected or topoSort dropped nodes")
  
    result
  }
}
