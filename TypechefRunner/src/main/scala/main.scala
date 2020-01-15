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


import java.io.{File, FileWriter}

import de.fosd.typechef.Frontend
import de.fosd.typechef.crewrite.{CInterAnalysisFrontend, ComposedWriter}
import de.fosd.typechef.options.FrontendOptions
import de.fosd.typechef.parser.c.{CASTEnv, TranslationUnit}



object main {
  def main(args: Array[String]): Unit = {
    var filename : String = null;

    Frontend.processFile(new FrontendOptions {
      de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)
//      files.add("/home/murad/typechef/main.c")
      files.add("/home/murad/intabs/intAbs/src/test/blink1/main.c")

//      systemIncludePath.add("/usr/include/")
//      systemIncludePath.add("/usr/lib/gcc/x86_64-linux-gnu/7/include/")
      systemIncludePath.add("/home/murad/typechef/root/usr/include/")
      systemIncludePath.add("/home/murad/typechef/root/usr/lib/gcc/x86_64-redhat-linux/4.4.4/include/")
      includedHeaders.add("/home/murad/typechef/plat_header_github.h")
      includedHeaders.add("/home/murad/typechef/garbage_test/header.h")
      simplifyPresenceConditions = true
      lexPrintToStdout = false
      writeDebugInterface = true;


      parserStatistics = true
      serializeAST = true
      filename = getSerializedASTFilename
//      dumpcfg = true

//       xtc = true
    })



    val ast = Frontend.loadSerializedAST(filename)

    val cf = new CInterAnalysisFrontend(ast)
//    val preds = cf.getAllSucc(cf.getTranslationUnit(), CASTEnv.createASTEnv(ast))
//    val s2 = cf.getAllSucc(preds.head._1, CASTEnv.createASTEnv(ast))
//    val s3 = cf.getAllSucc(s2.head._1, CASTEnv.createASTEnv(ast))

    val cw = new CustomWriter(new FileWriter(new File("hello.cfg")))
    cf.writeCFG("hello", new ComposedWriter(List(cw)))
  }

}
