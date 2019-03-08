package com.github.johnynek.bazel_deps.resolver

import com.github.johnynek.bazel_deps._

import scala.collection.mutable

sealed trait ResolvedNode

case class Replacement(name: String, actual: UnversionedCoordinate) extends ResolvedNode

case class ResolvedMavenCoordinate(
  group: MavenGroup,
  artifact: MavenArtifactId,
  version: Version,
  unversioned: UnversionedCoordinate,
  dependencies: Set[ResolvedMavenCoordinate],
  duplicates: Set[MavenCoordinate],
  shas: ResolvedShasValue
) extends ResolvedNode


/**
  *
  */
class ResolvedGraph(
  g: Graph[MavenCoordinate, Unit],
  duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
  shas: Map[MavenCoordinate, ResolvedShasValue],
  replacements: Seq[Replacements]
)(implicit opts: Options) extends Seq[ResolvedNode] {

  private[this] var nodes: Seq[ResolvedNode] = _

  override def length: Int = nodes.length

  override def apply(idx: Int): ResolvedNode = nodes.apply(idx)

  override def iterator: Iterator[ResolvedNode] = nodes.iterator

  {
    val seen: mutable.Set[UnversionedCoordinate] = mutable.Set()
    val unseen: mutable.Queue

    /*
     some recursive function that..
     - takes MavenCoordinate as input
     - guard condition on seen.contains(_)
     -
   */

  }

}
