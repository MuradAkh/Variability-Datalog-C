

import java.io.{File, FileWriter}

import de.fosd.typechef.Frontend
import de.fosd.typechef.crewrite.{CInterAnalysisFrontend, ComposedWriter}
import de.fosd.typechef.options.FrontendOptions
import de.fosd.typechef.parser.c.{CASTEnv, TranslationUnit}



object main {
  def main(args: Array[String]): Unit = {
    var filename : String = null;
    var home : String =  System.getenv("HOME");

    Frontend.processFile(new FrontendOptions {
      de.fosd.typechef.featureexpr.FeatureExprFactory.setDefault(de.fosd.typechef.featureexpr.FeatureExprFactory.bdd)
      files.add("../_temp/target.c")
      //files.add("/home/murad/intabs/intAbs/src/test/blink1/main.c")

//      systemIncludePath.add("/usr/include/")
    //  systemIncludePath.add("/usr/lib/gcc/x86_64-linux-gnu/7/include/")
      systemIncludePath.add(home + "/TypeChef-LinuxAnalysis2/data/systems/redhat/usr/include/")
      systemIncludePath.add(home + "/TypeChef-LinuxAnalysis2/data/systems/redhat/usr/lib/gcc/x86_64-redhat-linux/4.4.4/include/")
      includedHeaders.add("./headers/plat_header_github.h")
      quoteIncludePath.add("../test/headers/")
      includedHeaders.add("./headers/header.h")
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
    print(ast.toString)
    val cf = new CInterAnalysisFrontend(ast)
//    val preds = cf.getAllSucc(cf.getTranslationUnit(), CASTEnv.createASTEnv(ast))
//    val s2 = cf.getAllSucc(preds.head._1, CASTEnv.createASTEnv(ast))
//    val s3 = cf.getAllSucc(s2.head._1, CASTEnv.createASTEnv(ast))

    val cw = new CustomWriter(new FileWriter(new File("../_temp/output.cfg")))
    cf.writeCFG("hello", new ComposedWriter(List(cw)))
  }

}
