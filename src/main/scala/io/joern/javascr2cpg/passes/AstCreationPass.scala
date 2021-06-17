package io.joern.javascr2cpg.passes

import com.github.javaparser.ast.{CompilationUnit, PackageDeclaration}
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.ast.body.{MethodDeclaration, TypeDeclaration}
import com.github.javaparser.{JavaParser, ParserConfiguration}
import com.github.javaparser.symbolsolver.JavaSymbolSolver
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import com.github.javaparser.symbolsolver.resolution.typesolvers.{
  CombinedTypeSolver,
  JavaParserTypeSolver,
  ReflectionTypeSolver
}
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewMethod,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}

import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.jdk.CollectionConverters._
import scala.collection.mutable

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

  override def runOnPart(filename: String): Iterator[DiffGraph] = {
    val solver         = typeSolver()
    val symbolResolver = new JavaSymbolSolver(solver);

    val parserConfig = new ParserConfiguration().setSymbolResolver(symbolResolver)
    val parser       = new JavaParser(parserConfig)
    val r            = parser.parse(new java.io.File(filename))

    r.getResult.asScala match {
      case Some(result) if result.getParsed == Parsedness.PARSED =>
        new AstCreator(filename).createAst(result)
      case _ =>
        println("Cannot parse: " + filename)
        println("Parsedness: " + r.getResult.asScala.map(_.getParsed).getOrElse("None"))
        println("Problems: ")
        r.getProblems.asScala.foreach(println)
        Iterator()
    }
  }

}

class AstCreator(filename: String) {

  val stack: mutable.Stack[NewNode] = mutable.Stack()
  val diffGraph: DiffGraph.Builder  = DiffGraph.newBuilder

  def createAst(parserResult: CompilationUnit): Iterator[DiffGraph] = {

    parserResult.getPackageDeclaration.asScala.foreach { packageDecl =>
      val namespaceBlock = addNamespaceBlock(packageDecl, filename)
      stack.push(namespaceBlock)
    }

    parserResult.getTypes.asScala.zipWithIndex.foreach { case (typ, i) =>
      stack.push(addTypeDeclNode(typ, i + 1))
      typ.getMethods.asScala.foreach(m => addMethod(m, typ))
      stack.pop()
    }

    Iterator(diffGraph.build)
  }

  private def addNamespaceBlock(
      packageDecl: PackageDeclaration,
      filename: String
  ): NewNamespaceBlock = {
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val packageName  = packageDecl.getName.toString
    val namespaceBlock = NewNamespaceBlock()
      .name(packageName.split("\\.").lastOption.getOrElse(""))
      .fullName(packageName)
      .filename(absolutePath)
      .order(1)
    diffGraph.addNode(namespaceBlock)
    namespaceBlock
  }

  private def addTypeDeclNode(typ: TypeDeclaration[_], siblingNum: Int): NewTypeDecl = {
    val baseTypeFullNames = typ
      .asClassOrInterfaceDeclaration()
      .getExtendedTypes
      .asScala
      .map(_.resolve().getQualifiedName)
      .toList

    val typeDecl = NewTypeDecl()
      .name(typ.getNameAsString)
      .fullName(typ.getFullyQualifiedName.asScala.getOrElse(""))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .order(siblingNum)
      .filename(filename)
    diffGraph.addNode(typeDecl)
    diffGraph.addEdge(stack.top, typeDecl, EdgeTypes.AST)
    typeDecl
  }

  private def addMethod(
      methodDeclaration: MethodDeclaration,
      typeDecl: TypeDeclaration[_]
  ): Unit = {
    val fullName = methodFullName(typeDecl, methodDeclaration)
    val methodNode = NewMethod()
      .name(methodDeclaration.getNameAsString)
      .fullName(fullName)
    diffGraph.addNode(methodNode)
    diffGraph.addEdge(stack.top, methodNode, EdgeTypes.AST)
  }

  private def methodFullName(
      typeDecl: TypeDeclaration[_],
      methodDeclaration: MethodDeclaration
  ): String = {
    typeDecl.getFullyQualifiedName.asScala.getOrElse(
      ""
    ) + ":" + methodDeclaration.getNameAsString + ":" + methodDeclaration.getTypeAsString
  }

}
