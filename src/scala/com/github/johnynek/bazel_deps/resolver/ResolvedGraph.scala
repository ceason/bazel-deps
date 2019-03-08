package com.github.johnynek.bazel_deps.resolver

import com.github.johnynek.bazel_deps.Writer.TargetsError
import com.github.johnynek.bazel_deps._

import scala.collection.mutable
import scala.util.Try

sealed trait ResolvedNode

case class Replacement(
  replacement: ReplacementRecord,
  actual: UnversionedCoordinate
) extends ResolvedNode

case class ResolvedMavenCoordinate(
  coord: MavenCoordinate,
  dependencies: mutable.Set[ResolvedNode],
  duplicates: Set[Edge[MavenCoordinate, Unit]],
  shas: ResolvedShasValue,
  projectRecord: Option[ProjectRecord]
) extends ResolvedNode


object ResolvedGraph {
  def from(
    g: Graph[MavenCoordinate, Unit],
    duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
    shas: Option[Map[MavenCoordinate, ResolvedShasValue]],
    dependencies: Dependencies,
    replacements: Replacements
  )(implicit opts: Options): Try[Iterable[ResolvedNode]] = Try {
    /**
      * Check that all the exports are well-defined
      * TODO make sure to write targets for replaced nodes
      */
    val badExports: List[TargetsError.BadExport] =
      g.nodes.toList.flatMap { c =>
        val uv = c.unversioned
        dependencies.exportedUnversioned(uv, replacements) match {
          case Left(baddies) => List(TargetsError.BadExport(c.unversioned, baddies))
          case Right(_) => Nil
        }
      }

    if (badExports.nonEmpty) {
      sys.error(s"bad exports: ${badExports}")
    }

    val resolvedNodes: Map[MavenCoordinate, ResolvedMavenCoordinate] =
      g.nodes.map { c =>
        val r: Option[ProjectRecord] = for {
          m <- dependencies.toMap.get(c.group)
          projectRecord <- m.get(ArtifactOrProject(c.artifact.asString))
        } yield projectRecord

        val resolved = ResolvedMavenCoordinate(
          coord = c,
          dependencies = new mutable.HashSet(),
          duplicates = duplicates.getOrElse(c.unversioned, Set.empty),
          projectRecord = r,
          shas = shas.get(c)
        )
        c -> resolved
      }.toMap

    val replacementNodes: Map[UnversionedCoordinate, Replacement] =
      replacements.unversionedToReplacementRecord.map {
        case (k, v) => k -> Replacement(v, k)
      }

    // Connect using edges
    for {
      (_, rc) <- resolvedNodes
      edge <- g.edges.getOrElse(rc.coord, Set.empty)
      if edge.source == rc.coord
      dep = edge.destination
    } {
      if (replacementNodes contains dep.unversioned)
        rc.dependencies += replacementNodes(dep.unversioned)
      else
        rc.dependencies += resolvedNodes(dep)
    }

    val allNodes: mutable.Map[UnversionedCoordinate, ResolvedNode] = mutable.Map()
    allNodes ++= replacementNodes
    for (coord <- resolvedNodes.values) {
      if (!allNodes.contains(coord.coord.unversioned)) {
        allNodes += (coord.coord.unversioned -> coord)
      }
    }
    allNodes.values
  }
}