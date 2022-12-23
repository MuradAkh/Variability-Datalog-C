// This is a modfied version taken from typechef/crewrite/dotgraph


// TypeChef is a research project released as free software:
// you can redistribute it and/or modify it under the terms
// of the GNU Lesser General Public License as published by
// the Free Software Foundation, version 3 of the License.

// The Partial Preprocessor component of TypeChef builds on jcpp
// developed by Ben Mankin and released under Apache License 2.0
// see http://www.anarres.org/projects/jcpp/
// We massively modified the original implementation.

// The TypeChef project uses the SAT solver sat4j released
// as LGPL (downloaded by sbt). see http://www.sat4j.org/

// For convenience, we include *sbt* which is released under
// BSD license. see http://code.google.com/p/simple-build-tool/

// Experimentally the Xtc/SuperC parser is included as .jar
// file, which is released mostly under GPLv2.

// Possible contained case studies and examples are not
// covered by this license.

// TypeChef is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.


import de.fosd.typechef.parser.c._
import de.fosd.typechef.parser.c.PrettyPrinter
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.parser.c.FunctionDef
import java.io.Writer
import java.util

import de.fosd.typechef.crewrite.{CFGWriter, IOUtilities}

class CustomWriter(fwriter: Writer) extends IOUtilities with CFGWriter {

  /**
   * output format in CSV
   *
   * we distinguish nodes and edges, nodes start with "N" edges with "E"
   *
   * nodes have the following format:
   *
   * N;id;kind;line;name[::container];featureexpr;container
   *
   * * id is an identifier that only has a meaning within a file and that is not stable over multiple runs
   *
   * * kind is one of "function|function-inline|function-static|declaration|statement|expression|unknown"
   *   functions are distinguished into functions with an inline or a static modifier (inline takes precedence)
   *
   * * line refers to the starting position in the .pi file
   *
   * * name is either the name of a function or some debug information together with the name of the containing function.
   *   For functions and declarations, the name is used as a reference and can be used to match nodes across files.
   *   For expressions and statements, the name is used for debugging only and returns the first characters of the statement.
   *   In this case, the name is however followed by :: and the function name that can be used to extract hierarchy information.
   *   Note that function names should be unique in the entire system for each configuration (that is, there may be multiple
   *   functions with the same name but mutually exclusive feature expressions)
   *
   * * featureexpr describes the condition when the node is included
   *
   *
   *
   * edges do not have a line and title:
   *
   * E;sourceid;targetid;featureexpr
   *
   * they connect nodes within a file
   * ids refer to node ids within the file
   * nodeids are always declared before edges connecting them
   *
   * edges between files are not described in the output, but must be computed separately with an external linker
   * that matches nodes based on function/declaration names
   */

  private def asText(o: AST, containerName: String): String = o match {
    case FunctionDef(specs, decl, _, _) =>
      //functions are tagged as inline or static if that modifier occurs at all. not handling conditional
      //modifiers correctly yet
      (if (specs.map(_.entry).contains(InlineSpecifier())) "function-inline;"
      else if (specs.map(_.entry).contains(StaticSpecifier())) "function-static;"
      else "function;") +
        o.getPositionFrom.getLine + ";" + decl.getName + ";" + decl.toString
    case s: Statement => s"statement;${s.getPositionFrom.getLine};${printLine(s)}::$containerName"
    case e: Expr => s"expression;${e.getPositionFrom.getLine};${printLine(e)}::$containerName"
    case Declaration(_, initDecl) => s"declaration;${o.getPositionFrom.getLine};${initDecl.map(_.entry.getName).mkString(",")}"
    case x => s"unknown;${x.getPositionFrom.getLine};${printLine(x)}::$containerName"
  }

  private def printLine(ast: AST): String ={
    // esc(PrettyPrinter.print(ast))
    esc(ast.toString)
  }



  def writeEdge(source: AST, target: AST, fexpr: FeatureExpr) {
    fwriter.write("E;" + System.identityHashCode(source).toString().substring(0,4)+ ";" + System.identityHashCode(target).toString().substring(0,4) + ";" + fexpr.toTextExpr + "\n")
  }

  def writeNode(o: AST, fexpr: FeatureExpr, containerName: String) {
    fwriter.write("N;" + System.identityHashCode(o).toString().substring(0,4) + ";" + asText(o, containerName) + ";" + fexpr.toTextExpr + "\n")
  }

  def writeFooter() {
  }

  def writeHeader(title: String) {
  }

  private def esc(i: String) = {
    i.replace(";", "").
      replace("\n", " ")
  }

  def close() {
    fwriter.close()
  }
}