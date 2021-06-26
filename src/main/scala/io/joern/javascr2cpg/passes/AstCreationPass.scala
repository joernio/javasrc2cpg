package io.joern.javascr2cpg.passes

import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import com.github.javaparser.symbolsolver.resolution.typesolvers.{
  CombinedTypeSolver,
  JavaParserTypeSolver,
  ReflectionTypeSolver
}
import org.slf4j.LoggerFactory

import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.jdk.CollectionConverters._

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filenames.size))) {

  private val logger = LoggerFactory.getLogger(classOf[AstCreationPass])

  override def partIterator: Iterator[String] = filenames.iterator

  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    val solver         = typeSolver()
    val symbolResolver = new JavaSymbolSolver(solver);

    val parserConfig = new ParserConfiguration().setSymbolResolver(symbolResolver)
    val parser       = new JavaParser(parserConfig)
    val parseResult  = parser.parse(new java.io.File(filename))

    parseResult.getResult.asScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        new AstCreator(filename).createAst(result)
      case _ =>
        logger.warn("Cannot parse: " + filename)
        logger.warn("Problems: ", parseResult.getProblems.asScala.toList.map(_.toString))
        Iterator()
    }
  }

  private def typeSolver() = {
    val combinedTypeSolver   = new CombinedTypeSolver()
    val reflectionTypeSolver = new ReflectionTypeSolver()
    val javaParserTypeSolver = new JavaParserTypeSolver(codeDir)
    combinedTypeSolver.add(reflectionTypeSolver)
    combinedTypeSolver.add(javaParserTypeSolver)
    combinedTypeSolver
  }

}
