package com.github.johnynek.bazel_deps.resolver

import com.github.johnynek.bazel_deps.{Language, MavenCoordinate, Model, UnversionedCoordinate}

import scala.collection.mutable
import scala.io.Source


/**
  *
  */
object SingleFileWriter {
  def executeGenerate(g: Iterable[ResolvedNode])(implicit m: Model): Unit = {
    System.err.println("It's doing stuff, WHEE!!")
    val nodelist = g.toList

    val aliasFile: String = s"package(default_visibility = ['//:__subpackages__'])\n\n" + nodelist.map {
      case r@Replacement(replacement, actual, pr) =>
        s"""alias(
           |    name = "${aliasName(actual)}",
           |    actual = "${replacement.target.asString}",
           |)""".stripMargin

      case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, projectRecord) =>
        val alias = new StringBuilder
        alias ++= duplicatesComment(r) + "\n"
        alias ++=
          s"""alias(
             |    name = "${aliasName(coord.unversioned)}",
             |    actual = "@${repositoryName(coord.unversioned)}//${coord.unversioned.artifact.packaging}",
             |)""".stripMargin
        alias.mkString
    }.mkString("\n\n") + "\n"

    val lockfile: String = {
       val template = Source.fromInputStream(getClass.getResource(
         "/templates/singlefile_lockfile.bzl").openStream()).mkString

      val lines = nodelist.collect {
        case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, pr) ⇒
          val p = new mutable.ArrayBuffer[(String, Any)]
          // pom packaging exports dependants
          val ruleName = r.language match {
            case Language.Scala(_, _) ⇒
              "scala_import"
            case Language.Kotlin ⇒
              "kt_import"
            case Language.Java ⇒
              "java_import"
          }
          p += ("rule_name" → ruleName)
          for (url ← shas.binaryJar.url) {
            p += ("url" → url)
          }
          for (sha ← shas.binaryJar.sha256) {
            p += ("sha256" → sha.toHex)
          }
          for (sha ← shas.binaryJar.sha1) {
            p += ("sha1" → sha.toHex)
          }
          for (source ← shas.sourceJar) {
            for (url ← source.url) {
              p += ("src_url" → url)
            }
            for (sha ← source.sha256) {
              p += ("src_sha256" → sha.toHex)
            }
            for (sha ← source.sha1) {
              p += ("src_sha1" → sha.toHex)
            }
          }
          p += ("maven_coordinates" → coord.asString)
          p += ("packaging" → coord.artifact.packaging)
          val deps: List[String] = dependencies.toList.collect{
            case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, projectRecord) ⇒
              s"@${repositoryName(coord.unversioned)}//${coord.unversioned.artifact.packaging}"
            case r@Replacement(replacement, actual, projectRecord) ⇒
              replacement.target.asString
          }.sorted
          if (deps.nonEmpty) {
            p += ("deps" → deps)
          }

          s"""'${repositoryName(coord.unversioned)}':{\n        ${p.map{
            case (k, v: String) ⇒
              s"'$k':'$v'"
            case (k, vs: List[String]) ⇒
              s"'$k': [\n            ${vs.map{ v ⇒ s"'$v'"}.mkString(",\n            ")}\n        ]"
            case (k, v) ⇒
              sys.error(s"unrecognized type '$v'")
          }.mkString(",\n        ")}\n    },"""

      }.sorted
      s"""TRANSITIVITY = "${m.options.flatMap(_.transitivity.map(_.asString)).getOrElse("deps")}"
         |DEPENDENCIES = {
         |    ${lines.mkString("\n    ")}
         |}
       """.stripMargin + template + "\n"
    }

    //println(nodelist)
    print(aliasFile)
    print(lockfile)
    println(s"Number of nodes is '${nodelist.length}'")
  }

  def repositoryName(r: UnversionedCoordinate)(implicit m: Model): String = {
    val s = new StringBuilder
    for (prefix <- m.options.flatMap(_.namePrefix)) {
      s ++= s"${prefix.asString}"
    }
    s ++= s"${r.group.asString}_${r.artifact.artifactId}"
    for (classifier <- r.artifact.classifier.filterNot(_ == "jar")) {
      s ++= s"_$classifier"
    }
    s.mkString
      .replace(".", "_")
      .replace("-", "_")
  }

  def aliasName(c: UnversionedCoordinate)(implicit m: Model): String = {
    val name = new StringBuilder
    name ++= c.group.asString
    name ++= "/"
    name ++= c.artifact.artifactId
      .stripSuffix("_2.10")
      .stripSuffix("_2.11")
      .stripSuffix("_2.12")
      .stripSuffix("_2.13")
    if (c.artifact.classifier.nonEmpty) {
      name ++= "_" + c.artifact.classifier.get
    }
    name.mkString
  }

  def duplicatesComment(r: ResolvedMavenCoordinate)(implicit m: Model): String = {
    def replaced(c: MavenCoordinate): Boolean = m.getReplacements.get(c.unversioned).isDefined
    val coord = r.coord
    val isRoot = m.dependencies.roots(coord)
    val v = r.coord.version
    val vs = r.duplicates
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
          vs.filterNot(e => replaced(e.source)).map { e =>
            s"""# - ${e.destination.version.asString} wanted by ${e.source.asString}"""
          }.sorted.mkString("\n")
    }
  }

}
