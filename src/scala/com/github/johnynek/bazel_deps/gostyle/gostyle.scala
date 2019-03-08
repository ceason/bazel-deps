package com.github.johnynek.bazel_deps.gostyle

import scala.collection.JavaConverters._
import scala.util.Try

/**
  *
  */
sealed trait GoStyleError
object Panic extends GoStyleError
object Unspecified extends GoStyleError
case class GoStyleCatchableException(msg: String, throwable: Throwable) extends java.lang.RuntimeException(msg, throwable) with GoStyleError

object gostyle {

  def panic(msg: String): GoStyleError = {
    // - log 'msg'
    System.err.println(msg)

    // - print a stack trace
    val stacktrace = Thread.currentThread()
      .getStackTrace
      .map(_.toString)
      .mkString("\n")
    System.err.println(stacktrace)

    // - exit the JVM
    sys.exit(1)
    Panic
  }

  def error(msg: String): GoStyleError = {
    throw GoStyleCatchableException(msg, null)
  }

  implicit class GoStyleTry[T](val self: Try[T]) extends AnyVal {

    def ?(f: => GoStyleError = Unspecified): T = {
      if (self.isSuccess) {
        return self.get
      }
      val err = Try(f).recover{
        case e: GoStyleCatchableException => e
      }.get.asInstanceOf[GoStyleCatchableException]
      throw self.failed.map{
        case e: GoStyleCatchableException =>
          GoStyleCatchableException(s"${err.msg}: ${e.msg}", e.throwable)
        case e: RuntimeException =>
          GoStyleCatchableException(s"${err.msg}", e)
      }.get
    }
  }

}
