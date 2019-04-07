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

    val duplicates: List[String] = nodelist.collect {
      case r: ResolvedMavenCoordinate if r.duplicates.nonEmpty ⇒
        r
    }.sortBy(c ⇒ label(c.coord.unversioned)).map { coord: ResolvedMavenCoordinate ⇒


      val sb = new mutable.StringBuilder
      sb ++= s"""    "${label(coord.unversionedCoord)}\": [ # ${coord.coord.version.asString} \n"""
      for (dup ← duplicatesComment(coord).sorted) {
        sb ++= s"""        "${dup}",\n"""
      }
      sb ++= "    ],"
      sb.mkString
    }

    val lockfileContent: String = {
      val mavenWorkspaceName = m.getOptions
        .namePrefix.map(_.asString).getOrElse("maven")
        .stripSuffix("_")
      val template = Source.fromInputStream(getClass.getResource(
        "/templates/singlefile_backend.bzl").openStream()).mkString
        .replace("%{maven_workspace_name}", mavenWorkspaceName)

      val lines = nodelist.map { r ⇒
        val p = new mutable.ArrayBuffer[(String, Any)]
        var tags: mutable.ListBuffer[String] = new mutable.ListBuffer[String]
        //        if (!roots.contains(r.unversionedCoord)) {
        //          tags += "no-ide"
        //        }
        p += ("alias" → r.unversionedCoord.toBazelRepoName(m.getOptions.getNamePrefix))
        r match {
          case Replacement(replacement, actual, projectRecord) ⇒
            p += ("replacement" → replacement.target.asString)
          case ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, pr) ⇒
            p += ("artifact" → coord.asString)
            if (roots.contains(r.unversionedCoord)) {
              p += ("is_root" → true)
            }
            if (coord.artifact.packaging != "pom") {
              //              p += ("jar_stamp" → s"//${m.getOptions.getThirdPartyDirectory.asString}:${aliasName(r.unversionedCoord)}")
              for (sha ← shas.binaryJar.sha256) {
                p += ("sha256" → sha.toHex)
              }
              for (source ← shas.sourceJar) {
                for (sha ← source.sha256) {
                  p += ("sha256_src" → sha.toHex)
                }
              }
            }
            val deps: List[String] = dependencies.toList.map {
              case r: ResolvedMavenCoordinate ⇒
                label(r.coord.unversioned)
              case r: Replacement ⇒
                label(r.actual)
            }.sorted
            if (deps.nonEmpty) {
              p += ("deps" → deps.sorted)
            }
        }

        if (tags.nonEmpty) {
          p += ("tags" → tags.toList.sorted)
        }

        s"""'${label(r.unversionedCoord)}': {\n        ${
          p.map {
            case (k, v: String) ⇒
              s"'$k': '$v'"
            case (k, vs: List[String]) ⇒
              s"'$k': [\n            ${vs.map { v ⇒ s"'$v'," }.mkString("\n            ")}\n        ]"
            case (k, v: Boolean) ⇒
              s"'$k': ${if (v) "True" else "False"}"
            case (k, v) ⇒
              sys.error(s"unrecognized type '$v'")
          }.mkString(",\n        ")
        }\n    },"""
      }.map(_.replace(''', '"')).sorted

      val resolvers = for {
        server ← m.options.toList.flatMap(_.resolvers).flatten
      } yield
        s""""${server.url}","""

      s"""_DUPLICATES = {
         |${duplicates.mkString("\n")}
         |}
         |_REPOSITORIES = [
         |    ${resolvers.mkString("\n    ")}
         |]
         |_DEPENDENCIES = {
         |    ${lines.mkString("\n    ")}
         |}""".stripMargin + s"\n$template\n"
    }
    writeFile(outputPath, lockfileContent)

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

  def label(r: UnversionedCoordinate)(implicit m: Model): String = {
    val artifactId = r.artifact.artifactId match {
      case ScalaArtifactId(artifact, scalaBinaryVersion) ⇒
        artifact
      case other ⇒
        other
    }
    val sb = new mutable.StringBuilder
    sb ++= s"//${r.group.asString}:$artifactId"
    for (classifier ← r.artifact.classifier) {
      sb ++= s"/$classifier"
    }
    sb.mkString
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

  def duplicatesComment(r: ResolvedMavenCoordinate)(implicit m: Model): List[String] = {
    def replaced(c: MavenCoordinate): Boolean = m.getReplacements.get(c.unversioned).isDefined

    val coord = r.coord
    val isRoot = m.dependencies.roots(coord)
    val v = r.coord.version
    r.duplicates.toList.filterNot(e => replaced(e.source)).map { e ⇒
      s"""${e.destination.version.asString} wanted by ${e.source.asString}"""
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
