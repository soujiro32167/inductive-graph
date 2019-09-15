package com.soujiro.graph

sealed trait Graph[+N] {
  def nodes: List[N]
  def isEmpty: Boolean
  def gmap[M](f: Context[N] => Context[M]): Graph[M]
  def adjacency: List[(N, List[N])]
  def uniqueAdjacency[M >: N]: Map[M, Set[M]]
  def append[M >: N](from: M, to: M): Graph[M] =
    Cons(Context(Nil, from, List(to)),
      Cons(Context(Nil, to, Nil), this))
  def ::[M >: N](edge: (M, M)): Graph[M] = append(edge._1, edge._2)
  def grev: Graph[N]
}

object Graph {
  def empty: Graph[Nothing] = Empty

  def fromEdges[N](edges: List[(N, N)]): Graph[N] = edges.foldLeft[Graph[N]](Graph.empty){ case (g, e) => e :: g}
}

case class Context[+N](from: Adj[N], vertex: N, to: Adj[N])

case class Cons[+N](ctx: Context[N], rest: Graph[N]) extends Graph[N] {
  override def nodes: List[N] = ctx.vertex :: rest.nodes

  override def isEmpty: Boolean = false

  override def gmap[M](f: Context[N] => Context[M]): Graph[M] = Cons(f(ctx), rest.gmap(f))

  def uniqueAdjacency[M >: N]: Map[M, Set[M]] =
    copy[M](ctx, rest).adjacency.map{ case (k, v) => (k, v.toSet) }.toMap

  override def equals(obj: Any): Boolean = obj match {
    case other: Graph[N] => uniqueAdjacency == other.uniqueAdjacency
    case _ => false
  }

  override def adjacency: List[(N, List[N])] =
  (ctx.vertex -> ctx.to :: ctx.from.map(v => v -> List(ctx.vertex)) ++ rest.adjacency)
    .groupMapReduce(_._1)(_._2)(_ ++ _).toList

  override def grev: Graph[N] = gmap { case Context(f, v, t) => Context(t, v, f) }
}

case object Empty extends Graph[Nothing] {

  override def nodes: List[Nothing] = Nil

  override def isEmpty: Boolean = true

  override def gmap[M](f: Context[Nothing] => Context[M]): Graph[M] = this

  override def equals(obj: Any): Boolean = false

  override def adjacency: List[(Nothing, List[Nothing])] = Nil

  override def uniqueAdjacency[M >: Nothing]: Map[M, Set[M]] = Map.empty

  override def grev: Graph[Nothing] = this
}
