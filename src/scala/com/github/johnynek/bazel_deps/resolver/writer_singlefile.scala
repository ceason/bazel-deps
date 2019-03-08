package com.github.johnynek.bazel_deps.resolver

import com.github.johnynek.bazel_deps.{MavenCoordinate, Model, Options, UnversionedCoordinate}


/**
  *
  */
object SingleFileWriter {
  def executeGenerate(g: Iterable[ResolvedNode])(implicit m: Model): Unit = {
    System.err.println("It's doing stuff, WHEE!!")
    val nodelist = g.toList

    val aliasFile: String = nodelist
      .map(aliasfileFormat)
      .mkString("\n\n") + "\n"

    println(nodelist)
    print(aliasFile)
    println(s"Number of nodes is '${nodelist.length}'")
  }

  def repositoryName(r: UnversionedCoordinate)(implicit m: Model): String = {
    val s = new StringBuilder
    for (prefix <- m.options.flatMap(_.namePrefix)) {
      s ++= s"${prefix.asString}"
    }
    s ++= s"${r.group.asString}_${r.artifact.artifactId}"
    for (classifier <- r.artifact.classifier.filterNot(_ == "jar")){
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

  def duplicatesComment(r: ResolvedMavenCoordinate)(implicit m: Model): Option[String] = {
    if (r.duplicates.isEmpty) {
      return None
    }
    def replaced(c: MavenCoordinate): Boolean = m.getReplacements.get(c.unversioned).isDefined
    val coord = r.coord
    val isRoot = m.dependencies.roots(coord)
    val v = r.coord.version
    val vs = r.duplicates
    val status =
      if (isRoot) s"fixed to ${v.asString}"
      else if (r.duplicates.map(_.destination.version).max == v) s"promoted to ${v.asString}"
      else s"downgraded to ${v.asString}"

    Some(s"""# duplicates in ${coord.unversioned.asString} $status\n""" +
      vs.filterNot(e => replaced(e.source)).map { e =>
        s"""# - ${e.source.asString} wanted version ${e.destination.version.asString}\n"""
      }.toSeq.sorted.mkString(""))
  }


  def aliasfileFormat(n: ResolvedNode)(implicit m: Model): String = n match {
    case r@Replacement(replacement, actual) =>
      s"""alias(
         |    name = "${aliasName(actual)}",
         |    actual = "${replacement.target.asString}",
         |)""".stripMargin

    case r@ResolvedMavenCoordinate(coord, dependencies, duplicates, shas, projectRecord) =>
      val alias = new StringBuilder
      for (dupeComment <- duplicatesComment(r)) {
        alias ++= dupeComment
      }
      alias ++= s"""alias(
         |    name = "${aliasName(coord.unversioned)}",
         |    actual = "@${repositoryName(coord.unversioned)}//${coord.unversioned.artifact.packaging}",
         |)""".stripMargin
      alias.mkString
  }

  def lockfileFormat(n: ResolvedNode): String = ???
}
