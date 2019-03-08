package com.github.johnynek.bazel_deps.resolver

import cats.data.NonEmptyList
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
  dependencies: Set[ResolvedMavenCoordinate],
  duplicates: Set[MavenCoordinate],
  shas: ResolvedShasValue,
  processors: Set[ProcessorClass]
) extends ResolvedNode


object ResolvedGraph {
  def from(
    g: Graph[MavenCoordinate, Unit],
    duplicates: Map[UnversionedCoordinate, Set[Edge[MavenCoordinate, Unit]]],
    shas: Map[MavenCoordinate, ResolvedShasValue],
    dependencies: Dependencies,
    replacements: Replacements
  )(implicit opts: Options): Try[Seq[ResolvedNode]] = Try {
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

    val check: Either[NonEmptyList[TargetsError.BadExport], Unit] = badExports match {
      case h :: tail => Left(NonEmptyList(h, tail))
      case Nil => Right(())
    }

    type E[A] = Either[NonEmptyList[TargetsError], A]
    check.right.flatMap { _ =>
      /**
        * Here are all the explicit artifacts
        */
      val uvToVerExplicit: Map[UnversionedCoordinate, MavenCoordinate] =
        g.nodes.map { c => (c.unversioned, c) }.toMap

      /**
        * Here are any that are replaced, they may not appear above:
        */
      val uvToRep: Map[UnversionedCoordinate, ReplacementRecord] =
        replacements.unversionedToReplacementRecord

      /**
        * Here are all the unversioned artifacts we need to create targets for:
        */
      val allUnversioned: Set[UnversionedCoordinate] =
        uvToVerExplicit.keySet ++ uvToRep.keySet

      // return resolved or replaced nodes for all maven coords
      val allResolved: List[ResolvedNode] =
        allUnversioned.toList.map { uvCoord =>
          if (uvToRep contains uvCoord)
            Replacement(
              actual = uvCoord,
              replacement = uvToRep(uvCoord)
            )
          else
            ResolvedMavenCoordinate(
              coord = uvToVerExplicit(uvCoord),
              dependencies = ???,
              duplicates = ???,
              shas = ???,
              processors = ???
            )
        }
      Right(allResolved)
    }.fold(
      ex => sys.error(s"failed with ${ex}"),
      s => s
    )
  }
}