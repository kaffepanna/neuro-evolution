package se.randomserver.ne

import se.randomserver.ne.Graph

class TarjanSpec extends munit.FunSuite{

  def scc[V](nodes: Set[V], edges: Map[V, Set[V]]): Set[Set[V]] =
    Graph.tarjanSCC(nodes, edges.getOrElse(_, Set.empty))

  def assertSCC[V](actual: Set[Set[V]], expected: Set[Set[V]]): Unit =
    assert(actual == expected)    

  test("single node") {
    val nodes = Set(1)
    val edges = Map.empty[Int, Set[Int]]

    assertSCC(
      scc(nodes, edges),
      Set(Set(1))
    )
  }
  // 1 → 2 → 3
  test("linear dag") {
    val nodes = Set(1, 2, 3)
    val edges = Map(
      1 -> Set(2),
      2 -> Set(3)
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set(1), Set(2), Set(3))
    )
  }
  // 1 → 2 → 3 → 1
  test("simple cycle") {
    val nodes = Set(1, 2, 3)
    val edges = Map(
      1 -> Set(2),
      2 -> Set(3),
      3 -> Set(1)
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set(1, 2, 3))
    )
  }
  // 1 ↔ 2     3 ↔ 4
  test("two disjoint cycles") {
    val nodes = Set(1, 2, 3, 4)
    val edges = Map(
      1 -> Set(2),
      2 -> Set(1),
      3 -> Set(4),
      4 -> Set(3)
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set(1, 2), Set(3, 4))
    )
  }  
  // 1 → 2 → 3 → 1
  //     ↓
  //     4
  test("cycle with tail") {
    val nodes = Set(1, 2, 3, 4)
    val edges = Map(
      1 -> Set(2),
      2 -> Set(3, 4),
      3 -> Set(1)
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set(1, 2, 3), Set(4))
    )
  }
  // 1 → 2 → 3 → 1
  // 3 → 4 → 5 → 4
  test("nested SCCs") {
    val nodes = Set(1, 2, 3, 4, 5)
    val edges = Map(
      1 -> Set(2),
      2 -> Set(3),
      3 -> Set(1, 4),
      4 -> Set(5),
      5 -> Set(4)
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set(1, 2, 3), Set(4, 5))
    )
  }
  // 1 ↺
  // 2
  test("self loop") {
    val nodes = Set(1, 2)
    val edges = Map(
      1 -> Set(1)
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set(1), Set(2))
    )
  }
  // in → h1 → h2 → out
  //      ↑       ↓
  //      └───────┘
  test("recurrent neat motif") {
    val nodes = Set("in", "h1", "h2", "out")
    val edges = Map(
      "in"  -> Set("h1"),
      "h1"  -> Set("h2"),
      "h2"  -> Set("out"),
      "out" -> Set("h1")
    )

    assertSCC(
      scc(nodes, edges),
      Set(Set("h1", "h2", "out"), Set("in"))
    )
  }  
}
