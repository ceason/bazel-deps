package com.github.johnynek.bazel_deps.resolver

import com.github.johnynek.bazel_deps.Options

/**
  *
  */
object SingleFileWriter {
  def executeGenerate(g: Iterable[ResolvedNode])(implicit opts: Option[Options]): Unit = {
    System.err.println("It's doing stuff, WHEE!!")
    val nodelist = g.toList
    println(nodelist)
    println(s"Number of nodes is '${nodelist.length}'")
  }
}
