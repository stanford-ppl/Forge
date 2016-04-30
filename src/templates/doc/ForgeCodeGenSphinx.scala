package ppl.dsl.forge
package templates
package doc

import java.io.{File,PrintWriter,FileWriter}
import scala.tools.nsc.io.{Directory,Path}
import scala.reflect.SourceContext
import scala.virtualization.lms.common._

import java.util.{Calendar, TimeZone}

import core._
import shared._
import Utilities._

trait ForgeCodeGenSphinx extends ForgeCodeGenDocBase with ForgeGenReStructuredText {
  val IR: ForgeApplicationRunner with ForgeExp
  import IR._

  override lazy val targetName = "sphinx"

  lazy val bldDir    = docDir + File.separator + "build"
  lazy val srcDir    = docDir + File.separator + "source"
  lazy val staticDir = srcDir + File.separator + "_static"        // TODO: Unused for now
  lazy val templateDir = srcDir + File.separator + "_templates"   // TODO: Unused for now
  def opsDir = srcDir + File.separator + "ops"
  def fileExt = "rst"

  // Add links to types in method signatures
  override def quote(x: Exp[Any]): String = x match {
    case Def(Tpe(s,args,stage)) if docGroups.contains(x.asInstanceOf[Exp[DSLGroup]]) =>
      docref(relFileName(x.asInstanceOf[Exp[DSLGroup]])) + escapeSpecial(makeTpePars(args))

    case Def(TpeInst(t,args)) if docGroups.contains(t) =>
      docref(relFileName(t)) + makeTpePars(args)

    case Def(TpeClass(s,sig,args)) if docGroups.contains(x.asInstanceOf[Exp[DSLGroup]]) =>
      docref(relFileName(x.asInstanceOf[Exp[DSLGroup]])) + escapeSpecial(makeTpePars(args))

    case _ => super.quote(x)
  }

  override def printTable(stream: PrintWriter) {
    val ncols = tableContents.head.length
    val colLengths = List.tabulate(ncols){i => tableContents.map{row => row(i).length}.max }

    val rowSep = colLengths.map{len => "-"*(len+2)}.mkString("+", "+", "+")

    tableContents.foreach{row =>
      stream.println(rowSep)
      stream.println( row.zip(colLengths).map{case (col, len) => " " + col + " " + " "*(len-col.length)}.mkString("|", "|", "|") )
    }
    stream.println(rowSep)
    stream.println()
  }

  override def break(stream: PrintWriter) = stream.println(hrule())

  def emitIndex(
    typeclasses: Iterable[GroupDocumentation],
    typeparams: Iterable[GroupDocumentation],
    datastructs: Iterable[GroupDocumentation],
    objects: Iterable[GroupDocumentation],
    primitives: Iterable[GroupDocumentation],
    others: Iterable[GroupDocumentation]
  ): Unit = {
    Directory(Path(bldDir)).createDirectory()
    Directory(Path(srcDir)).createDirectory()
    Directory(Path(staticDir)).createDirectory()
    Directory(Path(templateDir)).createDirectory()

    val year = Calendar.getInstance(TimeZone.getDefault()).get(Calendar.YEAR)
    val lcdsl = dsl.toLowerCase()

    // Create the configuration for this DSL documentation project
    val conf = new PrintWriter(new FileWriter(srcDir+File.separator+"conf.py"))
    conf.println("import os")
    conf.println("import sys")
    conf.println("from sphinx.highlighting import PygmentsBridge")
    conf.println("from pygments.formatters.latex import LatexFormatter")
    conf.println("class CustomLatexFormatter(LatexFormatter):")
    conf.println("  def __init__(self, **options):")
    conf.println("    super(CustomLatexFormatter, self).__init__(**options)")
    conf.println("    self.verboptions = r\"formatcom=\\footnotesize\" ")
    conf.println("PygmentsBridge.latex_formatter = CustomLatexFormatter")

    conf.println()
    conf.println("templates_path = ['_templates']")
    conf.println("source_suffix = '.rst'")
    conf.println("master_doc = 'index'")
    conf.println("project = u'" + dsl + "'")
    conf.println("copyright = u'" + year +","+dslAuthor+"'")
    conf.println("version = '"+dslVersion+"'")
    conf.println("release = '"+dslVersion+"'")
    conf.println("highlight_language = 'none'")//'pygments.lexers.jvm.ScalaLexer'") // Lexer for language
    //conf.println("pygments_style = 'pygments.styles.monokai'")            // Style sheet
    conf.println("")
    conf.println()
    conf.println("#### HTML Info ###")
    conf.println("html_theme = 'sphinx_rtd_theme'")
    conf.println("html_static_path = ['_static']")
    conf.println("htmlhelp_basename = '" + dsl + "Doc'")
    conf.println()
    conf.println("### LaTex Info ###")
    conf.println("latex_documents = [")
    conf.println("  ('index', '"+dsl+".tex', u'"+dsl+" Documentation',")
    conf.println("  u'"+dslAuthor+"', 'manual'),")
    conf.println("]")
    conf.println("latex_elements = {")
    conf.println("  'classoptions': ',openany,oneside',")
    conf.println("  'babel': '\\\\usepackage[english]{babel}'") // ??????
    //conf.println("  'preamble': '\\\\usepackage[dvipsnames]{xcolor}'")
    conf.println("}")
    //conf.println("latex_additional_files = ['customcolors.sty'] ")
    conf.println()
    conf.println("### Manpage Info ###")
    conf.println("man_pages = [")
    conf.println("  ('index', '"+lcdsl+"', u'"+dsl+" Documentation',")
    conf.println("  [u'"+dslAuthor+"'], 1)")
    conf.println("]")
    conf.println()
    conf.println("### TexInfo ###")
    conf.println("texinfo_documents = [")
    conf.println("  ('index', '"+dsl+"', u'"+dsl+" Documentation',")
    conf.println("  u'"+dslAuthor+"', '"+dsl+"', 'One line description of project.',")
    conf.println("  'Miscellaneous'),")
    conf.println("]")
    conf.close()

    /*val sty = new PrintWriter(new FileWriter(srcDir+File.separator+"customcolors.sty"))
    colors.foreach{color =>
      sty.println(color + ":")
      sty.println("  parent: bodytext")
      sty.println("  textColor: " + color)
      sty.println()
    }
    sty.close()*/

    val css = new PrintWriter(new FileWriter(staticDir+File.separator+"colors.css"))
    colors.foreach{color => css.println("   ."+color+" { color:"+color+"; }" )}
    css.close()

    // --- Index
    val index = new PrintWriter(new FileWriter(srcDir+File.separator+"index.rst"))
    val heading = dsl+" " + dslVersion + " Documentation"
    index.println()
    index.println(sect(heading))
    index.println()
    index.println("Contents:")
    index.println()
    index.println(".. toctree::")
    index.println("   :maxdepth: 3")
    index.println()
    index.println("   intro")

    val intro = new PrintWriter(new FileWriter(srcDir+File.separator+"intro.rst"))
    intro.println(sect("Introduction"))
    intro.println()
    DSLBloc.getOrElse("<stub>").split(nl).map(_.trim()).foreach{line => intro.println(line) }
    intro.println()
    intro.println("This document was auto-generated using `Sphinx <http://www.sphinx-doc.org/en/stable/>`_.")
    intro.println("For corrections, post an issue on `GitHub Issues <https://github.com/stanford-ppl/Forge/issues/>`_ .")
    intro.println()
    intro.close()

    def emitGroupIndex(list: Iterable[GroupDocumentation], filename: String, title: String) {
      if (list.nonEmpty) {
        index.println("   " + filename)
        val pw = new PrintWriter(new FileWriter(srcDir+File.separator+filename+".rst"))
        pw.println(sect(title))
        pw.println()
        pw.println(".. toctree::")
        list.map(doc => fileName(doc.grp)).toArray.sorted.foreach{file => pw.println("   ops/"+file)}
        pw.close()
      }
    }

    emitGroupIndex(typeclasses, "typeclasses", "Type Classes")
    emitGroupIndex(datastructs, "datastructs", "Data Structures")
    emitGroupIndex(objects, "objects", "Objects")
    emitGroupIndex(primitives, "primitives", "Primitives")
    emitGroupIndex(typeparams, "typeparams", "Generic Methods")
    emitGroupIndex(others, "otherops", "Operations")

    //index.println(sect("Indices"))
    //index.println()
    //index.println("* :ref:`genindex`")
    //index.println("* :ref:`modindex`")
    //index.println("* :ref:`search`")
    index.println()
    index.close()
  }

  def emitGroupHeader(grp: String, desc: GrpDescription, stream: PrintWriter): Unit = {
    stream.println()
    colors.foreach{color => stream.println(".. role:: " + color) }
    stream.println()
    stream.println(label(grp))
    stream.println()
    stream.println(sect(grp))
    stream.println()
    desc.desc.foreach{line => stream.println(line) }
    stream.println()
    // TODO: Examples
    // TODO: Include fields?
    /*if (desc.fields.nonEmpty) {
      stream.println()
      stream.println(subsect("Fields"))
      desc.fields.foreach{case (field,desc) => stream.println()}*/

  }
  def emitOpFamilyHeader(style: String, stream: PrintWriter): Unit = {
    stream.println(subsect(style))
    stream.println()
  }

  def emitOpDescription(desc: OpDescription, signature: String, stream: PrintWriter): Unit = {
    stream.println(signature)
    stream.println(desc.desc)
    stream.println()
    desc.args.foreach{case (arg,desc) => if (desc != "") stream.println("\t* " + bold(arg) + " \\- " + desc) }
    desc.returns.foreach{desc => stream.println("\t* " + bold("returns") + " " + desc) }

    if (desc.examples.nonEmpty) stream.println()
    desc.examples.foreach{case (example, expl) =>
      stream.println(code(example))
      stream.println()
      stream.println(expl)
      stream.println()
    }
  }

  def emitOpFamilyFooter(style: String, stream: PrintWriter): Unit = {
    stream.println()
  }
  def emitGroupFooter(grp: String, stream: PrintWriter): Unit = {}
}
