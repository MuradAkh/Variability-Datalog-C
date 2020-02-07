

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
