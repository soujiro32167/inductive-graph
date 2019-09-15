package com.soujiro.graph

import org.scalatest.{Matchers, WordSpec}

class GraphTest extends WordSpec with Matchers {
  "basics" in {
    Empty.isEmpty shouldEqual true

    Cons(Context(Nil, 1, Nil), Empty).isEmpty shouldEqual false

    Empty shouldEqual Empty

    Empty should not equal 1

    Cons(Context(Nil, 1, Nil), Empty) should not equal 1
  }

  "Graph construction" should {
    "simple graph" in {
      val g = Cons(Context(List(2, 3), 1, List(2)),
        Cons(Context(Nil, 2, List(3)),
          Cons(Context(Nil, 3, Nil),
            Empty
          )
        )
      )

      g.nodes shouldEqual List(1, 2, 3)
    }

    "in any order" in {
      val g1 = Cons(Context(List(2, 3), 1, List(2)),
        Cons(Context(Nil, 2, List(3)),
          Cons(Context(Nil, 3, Nil),
            Empty
          )
        )
      )

      val g2 = Cons(Context(List(2), 3, List(1)),
        Cons(Context(List(1), 2, List(1)),
          Cons(Context(Nil, 1, Nil),
            Empty
          )
        )
      )

      g1.uniqueAdjacency shouldEqual g2.uniqueAdjacency
      g1 shouldEqual g2
      g1.nodes.toSet shouldEqual g2.nodes.toSet

    }

    "using append" in {
      Graph.empty.append(1, 2) shouldEqual 1 -> 2 :: Graph.empty

      val actual = 2 -> 1 :: 3 -> 1 :: 1 -> 2 :: 2 -> 3 :: Graph.empty

      val expected =
        Cons(Context(List(2, 3), 1, List(2)),
          Cons(Context(Nil, 2, List(3)),
            Cons(Context(Nil, 3, Nil),
              Empty
          )
        )
      )

      actual shouldEqual expected
    }

    "using fromEdges" in {
      val actual = Graph.fromEdges(
        List(
          "a" -> "b",
          "a" -> "c",
          "a" -> "d",
          "d"  -> "a"
        )
      )

      val expected = Cons(
        Context(List("d"), "a", List("d", "b", "c")),
        Cons(Context(Nil, "b", Nil),
          Cons(Context(Nil, "c", Nil),
            Cons(Context(Nil, "d", Nil),
              Empty
            ))))

      actual shouldEqual expected
    }
  }

  "Graph operations" should {
    "grev" in {
      val actual = Graph.fromEdges(
        List(
          "a" -> "b",
          "a" -> "c",
          "a" -> "d",
          "d"  -> "a"
        )
      ).grev

      val expected = Graph.fromEdges(
        List(
          "b" -> "a",
          "c" -> "a",
          "d" -> "a",
          "a" -> "d"
        )
      )

      actual shouldEqual expected

      Empty.grev shouldEqual Empty
    }

    "equality" in {
      val g = Cons(Context(Nil, 1, Nil), Empty)

      g should not equal Graph.empty

      val g2 = Cons(Context(List(1), 1, Nil), Empty)

      g2 should not equal g
    }
  }
}
