package se.randomserver.ne

class KhanSpec extends munit.FunSuite {
  def isValidTopo[V](order: Seq[V], edges: V => Set[V]): Boolean = {
    val pos = order.zipWithIndex.toMap
    order.forall { u =>
      edges(u).forall(v => pos(u) < pos(v))
    }
  }

  test("kahnTopoSort: empty graph") {
    val nodes = Set.empty[Int]
    val edges = (_: Int) => Set.empty[Int]

    val result = Graph.kahnTopoSort(nodes, edges)
    assertEquals(result, Seq.empty)
  }

  test("kahnTopoSort: single node") {
    val nodes = Set(1)
    val edges = (_: Int) => Set.empty[Int]

    val result = Graph.kahnTopoSort(nodes, edges)
    assertEquals(result, Seq(1))
  }

  test("kahnTopoSort: simple chain") {
    val nodes = Set("A", "B", "C")
    val edges: String => Set[String] = {
      case "A" => Set("B")
      case "B" => Set("C")
      case _   => Set.empty
    }

    val result = Graph.kahnTopoSort(nodes, edges)

    assertEquals(result.toSet, nodes)
    assert(isValidTopo(result, edges))
  }
  /* 
        A
       / \
      B   C
       \ /
        D
   */
  test("kahnTopoSort: diamond graph") {
    val nodes = Set("A", "B", "C", "D")
    val edges: String => Set[String] = {
      case "A" => Set("B", "C")
      case "B" => Set("D")
      case "C" => Set("D")
      case _   => Set.empty
    }

    val result = Graph.kahnTopoSort(nodes, edges)

    assertEquals(result.toSet, nodes)
    assert(isValidTopo(result, edges))
  }

  test("kahnTopoSort: disconnected components") {
    val nodes = Set(1, 2, 3, 4)
    val edges: Int => Set[Int] = {
      case 1 => Set(2)
      case 3 => Set(4)
      case _ => Set.empty
    }

    val result = Graph.kahnTopoSort(nodes, edges)

    assertEquals(result.toSet, nodes)
    assert(isValidTopo(result, edges))
  }

  test("kahnTopoSort: isolated node") {
    val nodes = Set("A", "B")
    val edges: String => Set[String] = {
      case "A" => Set("B")
      case _   => Set.empty
    }

    val result = Graph.kahnTopoSort(nodes, edges)

    assertEquals(result.toSet, nodes)
    assert(isValidTopo(result, edges))
  }

  test("kahnTopoSort: cycle throws") {
    val nodes = Set(1, 2)
    val edges: Int => Set[Int] = {
      case 1 => Set(2)
      case 2 => Set(1)
    }

    intercept[IllegalArgumentException] {
      Graph.kahnTopoSort(nodes, edges)
    }
  }

  test("kahnTopoSort: self loop") {
    val nodes = Set(1)
    val edges: Int => Set[Int] = {
      case 1 => Set(1)
    }

    intercept[IllegalArgumentException] {
      Graph.kahnTopoSort(nodes, edges)
    }
  }
}
