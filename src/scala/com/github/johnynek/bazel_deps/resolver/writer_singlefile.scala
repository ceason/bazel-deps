package com.github.johnynek.bazel_deps.resolver

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}

import com.github.johnynek.bazel_deps.{MavenCoordinate, Model, UnversionedCoordinate}

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex


/**
  *
  */
object SingleFileWriter {
  def executeGenerate(g: Iterable[ResolvedNode], outputPath: String)(implicit m: Model): Unit = {
    val nodelist = g.toList

    val roots: Set[UnversionedCoordinate] =
      m.dependencies.roots.map(_.unversioned)

    val aliasFileContent: String = {
      val comments: Map[UnversionedCoordinate, String] =
        nodelist.collect {
          case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, projectRecord) =>
            coord.unversioned → duplicatesComment(r)
        }.toMap

      s"package(default_visibility = ['//:__subpackages__'])\n\n" + nodelist.map {
        case r@Replacement(replacement, actual, pr) =>
          actual
        case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, projectRecord) =>
          coord.unversioned
      }.filter { coord ⇒
        // only include root nodes when 'strict visibility' is enabled
        if (m.getOptions.strictVisibility.exists(_.enabled))
          roots.contains(coord)
        else
          true
      }.map { coord ⇒
        val alias = new StringBuilder
        for (comment ← comments.get(coord)) {
          alias ++= s"$comment\n"
        }
        alias ++=
          s"""java_library(
             |    name = "${aliasName(coord)}",
             |    exports = ["@${repositoryName(coord)}"],
             |)""".stripMargin
        alias.mkString
      }.mkString("\n\n") + "\n"
    }

    val lockfileContent: String = {
      val template = Source.fromInputStream(getClass.getResource(
        "/templates/singlefile/lockfile.bzl").openStream()).mkString

      val lines = nodelist.collect {
        case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, pr) ⇒
          val p = new mutable.ArrayBuffer[(String, Any)]
          p += ("name" → repositoryName(coord.unversioned))
          p += ("artifact" → coord.asString)
          for (sha ← shas.binaryJar.sha256) {
            p += ("sha256" → sha.toHex)
          }
          for (source ← shas.sourceJar) {
            for (sha ← source.sha256) {
              p += ("sha256_src" → sha.toHex)
            }
          }
          val deps: List[String] = dependencies.toList.map {
            case r: ResolvedMavenCoordinate ⇒
              s"@${repositoryName(r.coord.unversioned)}"
            case r: Replacement ⇒
              s"@${repositoryName(r.actual)}"
          }.sorted
          if (deps.nonEmpty) {
            p += ("deps" → deps)
          }
          s"""'${withoutAnyVersions(coord.unversioned)}': {\n        ${
            p.map {
              case (k, v: String) ⇒
                s"'$k': '$v'"
              case (k, vs: List[String]) ⇒
                s"'$k': [\n            ${vs.map { v ⇒ s"'$v'," }.mkString("\n            ")}\n        ]"
              case (k, v) ⇒
                sys.error(s"unrecognized type '$v'")
            }.mkString(",\n        ")
          }\n    },"""

      }.map(_.replace(''', '"')).sorted

      val resolvers = for {
        server ← m.options.toList.flatMap(_.resolvers).flatten.sortBy(_.id)
      } yield
        s""""${server.id}": "${server.url}","""

      s"""_REPOSITORIES = {
         |    ${resolvers.mkString("\n    ")}
         |}
         |_DEPENDENCIES = {
         |    ${lines.mkString("\n    ")}
         |}""".stripMargin + s"\n$template\n"
    }

    val aliasFile = Paths.get(outputPath, "BUILD")
    val libFile = Paths.get(outputPath, "internal.bzl")
    val lockfile = Paths.get(outputPath, "dependencies.bzl")

    new File(outputPath).mkdirs()
    writeFile(libFile.toString, Source.fromInputStream(getClass.getResource(
      "/templates/singlefile/internal.bzl").openStream()).mkString
      .replace("%{output_path}", m.getOptions.getThirdPartyDirectory.asString))
    writeFile(lockfile.toString, lockfileContent)
    writeFile(aliasFile.toString, aliasFileContent)

    println(s"Number of nodes is '${nodelist.length}'")
  }

  val ScalaArtifactId: Regex = "(.*)_(2\\.1[0-9])".r

  /// remove even scala binary version
  def withoutAnyVersions(r: UnversionedCoordinate): String = {
    val sb = new mutable.StringBuilder
    val artifactId = r.artifact.artifactId match {
      case ScalaArtifactId(artifact, scalaBinaryVersion) ⇒
        artifact
      case other ⇒
        other
    }
    sb ++= s"${r.group.asString}:$artifactId"
    if (r.artifact.packaging != "jar" || r.artifact.classifier.nonEmpty) {
      sb ++= ":" + r.artifact.packaging
    }
    for (classifier ← r.artifact.classifier) {
      sb ++= ":" + classifier
    }
    sb.mkString
  }

  def repositoryName(r: UnversionedCoordinate)(implicit m: Model): String = {
    val name = withoutAnyVersions(r)
      .replace(".", "_")
      .replace("-", "_")
      .replace(":", "_")
    m.getOptions.getNamePrefix.asString + name
  }

  def aliasName(c: UnversionedCoordinate)(implicit m: Model): String = {
    val artifactId = c.artifact.artifactId match {
      case ScalaArtifactId(artifact, scalaBinaryVersion) ⇒
        artifact
      case other ⇒
        other
    }
    val name = new StringBuilder
    name ++= s"${c.group.asString}/$artifactId"
    for (classifier ← c.artifact.classifier) {
      name ++= s"/$classifier"
    }
    name.mkString
  }

  def duplicatesComment(r: ResolvedMavenCoordinate)(implicit m: Model): String = {
    def replaced(c: MavenCoordinate): Boolean = m.getReplacements.get(c.unversioned).isDefined

    val coord = r.coord
    val isRoot = m.dependencies.roots(coord)
    val v = r.coord.version
    r.duplicates.toList match {
      case Nil ⇒
        s"# ${v.asString}"
      case vs ⇒
        val status =
          if (isRoot) "fixed"
          else if (vs.map(_.destination.version).max == v) "promoted"
          else "downgraded"
        s"""# ${v.asString} ($status)
           |# duplicates:\n""".stripMargin +
          vs.filterNot(e => replaced(e.source)).map { e ⇒
            s"""# - ${e.destination.version.asString} wanted by ${e.source.asString}"""
          }.sorted.mkString("\n")
    }
  }


  def writeFile(path: String, content: String): Unit = {
    // if file exists at path, read it in and compare content
    if (Files.exists(Paths.get(path))) {
      if (Source.fromFile(path).mkString == content) {
        // if file is identical don't do anything
        return
      }
    }
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(content)
    bw.close()
  }
}
