package com.github.johnynek.bazel_deps.resolver

import com.github.johnynek.bazel_deps.Writer.TargetsError
import com.github.johnynek.bazel_deps._

import scala.collection.mutable
import scala.util.Try

sealed trait ResolvedNode {
  def unversionedCoord: UnversionedCoordinate
}

case class Replacement(
  replacement: ReplacementRecord,
  actual: UnversionedCoordinate,
  projectRecord: Option[ProjectRecord]
) extends ResolvedNode {
  override def unversionedCoord: UnversionedCoordinate = actual
}

case class ResolvedMavenCoordinate(
  coord: MavenCoordinate,
  dependencies: mutable.Set[ResolvedNode],
  duplicates: Set[Edge[MavenCoordinate, Unit]],
  shas: ResolvedShasValue,
  projectRecord: Option[ProjectRecord]
) extends ResolvedNode {

  override def unversionedCoord: UnversionedCoordinate = coord.unversioned

  val language: Language = {
    projectRecord
      .map(_.lang)
      .orElse {
        val scalaBinaryVersion = ".*_(2\\.1[0-9])".r
        coord.artifact.artifactId match {
          case scalaBinaryVersion(v) ⇒
            Some(Language.Scala(Version(v), mangle = false))
          case _ ⇒
            None
        }
      }.getOrElse(Language.Java)
  }

}


object ResolvedGraph {
  def from(
    g: Graph[MavenCoordinate, Unit],
    duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
    shas: Map[MavenCoordinate, ResolvedShasValue],
    dependencies: Dependencies,
    replacements: Replacements
  ): Try[Iterable[ResolvedNode]] = Try {
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

    val replacementNodes: Map[UnversionedCoordinate, Replacement] = {
      replacements.unversionedToReplacementRecord.map {
        case (k, v) =>
          val r: Option[ProjectRecord] = for {
            m <- dependencies.toMap.get(k.group)
            projectRecord <- m.get(ArtifactOrProject(k.artifact.asString))
          } yield projectRecord
          k -> Replacement(v, k, r)
      }
    }

    val resolvedNodes: Map[MavenCoordinate, ResolvedMavenCoordinate] =
      g.nodes
        .filterNot(c => replacementNodes.contains(c.unversioned))
        .map { c =>
          val r: Option[ProjectRecord] = for {
            m <- dependencies.toMap.get(c.group)
            projectRecord <- m.get(ArtifactOrProject(c.artifact.asString))
          } yield projectRecord

          val resolved = ResolvedMavenCoordinate(
            coord = c,
            dependencies = new mutable.HashSet(),
            duplicates = duplicates.getOrElse(c.unversioned, Set.empty),
            projectRecord = r,
            shas = shas(c)
          )
          c -> resolved
        }.toMap

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