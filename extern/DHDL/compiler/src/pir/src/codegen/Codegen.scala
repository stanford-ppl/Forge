package dhdl.codegen

import dhdl.graph._
import dhdl.graph.traversal.Traversal
import dhdl.Design

import scala.collection.mutable.Set
import java.io.PrintWriter
import java.io.{File, FileInputStream, FileOutputStream}

abstract class Codegen(implicit design: Design) extends Traversal {
  val ext: String
  val pwd = new File(".").getAbsolutePath().dropRight(2)
  def clearOldFiles = true
  def outdir = s"${pwd}/out/$ext"

  def fp(pw: PrintWriter, a: Any) = pw.println(a)

  def filesDir = s"$pwd/src/codegen/$ext/files"

  def quote(x: Any): String = {
    x match {
      case Some(v) => v.toString
      case _ => x.toString
    }
  }

  override def initPass() = {
    val file = new File(outdir)
    if (clearOldFiles) {
      if (file.exists) {
        deleteFiles(file)
      }
    }
    file.mkdirs()
    isInit = true
  }

  def deleteFiles(file: File): Unit = {
    if (file.isDirectory) {
      Option(file.listFiles).map(_.toList).getOrElse(Nil).foreach(deleteFiles(_))
    }
    file.delete
  }

  def copyFile(src: String, dst: String) = {
    val srcFile = new File(src)
    val dstFile = new File(dst)
    new FileOutputStream(dstFile)
            .getChannel()
            .transferFrom (
              new FileInputStream(srcFile).getChannel(), 0, Long.MaxValue
            )
  }

  def copyDir(srcDirFile: File, dstDirFile: File): Unit = {
    for (f <- srcDirFile.listFiles) {
      if (f.isDirectory) {
        val dstDir = new File(s"${dstDirFile.getAbsolutePath}/${f.getName}")
        dstDir.mkdirs()
        copyDir(f, dstDir)
      } else {
        val dst = s"${dstDirFile.getAbsolutePath()}/${f.getName}"
        val src = f.getAbsolutePath()
        copyFile(src, dst)
      }
    }
  }

  def copyDir(srcDir: String, dstDir: String): Unit = {
    val srcDirFile = new File(srcDir)
    val srcDirName = srcDir.split("/").last
    val dstDirFile = new File(s"$dstDir/$srcDirName")
    dstDirFile.mkdirs()

    for (f <- srcDirFile.listFiles) {
      if (f.isDirectory) {
        val dstDir = new File(s"${dstDirFile.getAbsolutePath}/${f.getName}")
        dstDir.mkdirs()
        copyDir(f, dstDir)
      } else {
        val dst = s"${dstDirFile.getAbsolutePath()}/${f.getName}"
        val src = f.getAbsolutePath()
        copyFile(src, dst)
      }
    }
  }
}
