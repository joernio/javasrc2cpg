package io.joern.javascr2cpg.passes

import com.github.javaparser.ast.CompilationUnit
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
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass

import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.jdk.CollectionConverters._

class AstCreationPass(codeDir: String, filenames: List[String], cpg: Cpg, keyPool: IntervalKeyPool)
    extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(filenames.size))) {

  override def partIterator: Iterator[String] = filenames.iterator

  private def typeSolver() = {
    val combinedTypeSolver   = new CombinedTypeSolver()
    val reflectionTypeSolver = new ReflectionTypeSolver()
    val javaParserTypeSolver = new JavaParserTypeSolver(codeDir)
    combinedTypeSolver.add(reflectionTypeSolver)
    combinedTypeSolver.add(javaParserTypeSolver)
    combinedTypeSolver
  }

  override def runOnPart(part: String): Iterator[DiffGraph] = {
    val solver         = typeSolver()
    val symbolResolver = new JavaSymbolSolver(solver);

    val parserConfig = new ParserConfiguration().setSymbolResolver(symbolResolver)
    val parser       = new JavaParser(parserConfig)
    val r            = parser.parse(new java.io.File(part))

    r.getResult.asScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        createAst(result, part)
      case _ =>
        println("Cannot parse: " + part)
        println("Parsedness: " + r.getResult.asScala.map(_.getParsed).getOrElse("None"))
        println("Problems: ")
        r.getProblems.asScala.foreach(println)
        Iterator()
    }
  }

  private def createAst(parserResult: CompilationUnit, filename: String): Iterator[DiffGraph] = {
    println(parserResult.getTypes)

    val diffGraph    = DiffGraph.newBuilder
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    parserResult.getPackageDeclaration.asScala.foreach { packageDecl =>
      val packageName = packageDecl.getName.toString
      val namespaceBlock = nodes
        .NewNamespaceBlock()
        .name(packageName.split("\\.").lastOption.getOrElse(""))
        .fullName(packageName)
        .filename(absolutePath)
        .order(1)

      diffGraph.addNode(namespaceBlock)
    }

    Iterator(diffGraph.build)
  }

}