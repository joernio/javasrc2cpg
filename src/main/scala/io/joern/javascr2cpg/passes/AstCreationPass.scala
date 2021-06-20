package io.joern.javascr2cpg.passes

import com.github.javaparser.ast.{CompilationUnit, PackageDeclaration}
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.ast.body.{MethodDeclaration, Parameter, TypeDeclaration}
import com.github.javaparser.ast.stmt.{
  AssertStmt,
  BlockStmt,
  BreakStmt,
  ContinueStmt,
  EmptyStmt,
  ExplicitConstructorInvocationStmt,
  ExpressionStmt,
  ForEachStmt,
  ForStmt,
  IfStmt,
  LabeledStmt,
  LocalClassDeclarationStmt,
  LocalRecordDeclarationStmt,
  ReturnStmt,
  SwitchStmt,
  SynchronizedStmt,
  ThrowStmt,
  TryStmt,
  UnparsableStmt,
  WhileStmt,
  YieldStmt
}
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
  NewMethodParameterIn,
  NewMethodReturn,
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
      typ.getMethods.asScala.zipWithIndex.foreach { case (m, i) => addMethod(m, typ, i + 1) }
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
    stack.headOption.foreach(head => diffGraph.addEdge(head, typeDecl, EdgeTypes.AST))
    typeDecl
  }

  private def addMethod(
      methodDeclaration: MethodDeclaration,
      typeDecl: TypeDeclaration[_],
      childNum: Int
  ): Unit = {
    val methodNode = addMethodNode(methodDeclaration, typeDecl, childNum)
    stack.push(methodNode)
    methodDeclaration.getParameters.asScala.zipWithIndex.foreach { case (p, i) =>
      addParameter(p, i + 1)
    }
    val methodReturnNode = addMethodReturnNode(methodDeclaration)
    diffGraph.addEdge(methodNode, methodReturnNode, EdgeTypes.AST)

    addMethodBody(methodDeclaration)

    stack.pop()
  }

  private def addMethodBody(methodDeclaration: MethodDeclaration) = {
    methodDeclaration.getBody.asScala.foreach { body =>
      body.getStatements.asScala.foreach {
        case x: AssertStmt                        =>
        case x: BlockStmt                         =>
        case x: BreakStmt                         =>
        case x: ContinueStmt                      =>
        case x: EmptyStmt                         =>
        case x: ExplicitConstructorInvocationStmt =>
        case x: ExpressionStmt                    =>
        case x: ForEachStmt                       =>
        case x: ForStmt                           =>
        case x: IfStmt                            =>
        case x: LabeledStmt                       =>
        case x: LocalClassDeclarationStmt         =>
        case x: LocalRecordDeclarationStmt        =>
        case x: ReturnStmt                        =>
        case x: SwitchStmt                        =>
        case x: SynchronizedStmt                  =>
        case x: ThrowStmt                         =>
        case x: TryStmt                           =>
        case x: UnparsableStmt                    =>
        case x: WhileStmt                         =>
        case x: YieldStmt                         =>
        case _                                    =>
      }
    }
  }

  private def addMethodReturnNode(methodDeclaration: MethodDeclaration) = {
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameters.size + 2)
        .typeFullName(methodDeclaration.getType.resolve().describe())
        .code(methodDeclaration.getTypeAsString)
        .lineNumber(methodDeclaration.getType.getBegin.map(x => new Integer(x.line)).asScala)
    diffGraph.addNode(methodReturnNode)
    methodReturnNode
  }

  private def addMethodNode(
      methodDeclaration: MethodDeclaration,
      typeDecl: TypeDeclaration[_],
      childNum: Int
  ) = {
    val fullName = methodFullName(typeDecl, methodDeclaration)
    val code     = methodDeclaration.getDeclarationAsString().trim
    val methodNode = NewMethod()
      .name(methodDeclaration.getNameAsString)
      .fullName(fullName)
      .code(code)
      .signature(methodDeclaration.getTypeAsString + paramListSignature(methodDeclaration))
      .isExternal(false)
      .order(childNum)
      .filename(filename)
      .lineNumber(methodDeclaration.getBegin.map(x => new Integer(x.line)).asScala)
    diffGraph.addNode(methodNode)
    stack.headOption.foreach(head => diffGraph.addEdge(head, methodNode, EdgeTypes.AST))
    methodNode
  }

  private def addParameter(parameter: Parameter, childNum: Int): Unit = {
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .typeFullName(parameter.getType.resolve().describe())
      .order(childNum)
      .lineNumber(parameter.getBegin.map(x => new Integer(x.line)).asScala)
      .columnNumber(parameter.getBegin.map(x => new Integer(x.column)).asScala)
    stack.headOption.foreach(head => diffGraph.addEdge(head, parameterNode, EdgeTypes.AST))
  }

  private def methodFullName(
      typeDecl: TypeDeclaration[_],
      methodDeclaration: MethodDeclaration
  ): String = {
    val typeName = typeDecl.getFullyQualifiedName.asScala.getOrElse(
      ""
    )
    typeName + "." + methodDeclaration.getNameAsString + ":" + methodDeclaration.getTypeAsString + paramListSignature(
      methodDeclaration
    )
  }

  private def paramListSignature(methodDeclaration: MethodDeclaration) = {
    val paramTypes = methodDeclaration.getParameters.asScala.map(_.getType.resolve().describe())
    "(" + paramTypes.mkString(",") + ")"
  }

}
