package io.joern.javascr2cpg.passes

import com.github.javaparser.ast.{CompilationUnit, Node, NodeList, PackageDeclaration}
import com.github.javaparser.ast.body.{MethodDeclaration, Parameter, TypeDeclaration}
import com.github.javaparser.ast.expr.{
  AnnotationExpr,
  ArrayAccessExpr,
  ArrayInitializerExpr,
  AssignExpr,
  BinaryExpr,
  CastExpr,
  ClassExpr,
  ConditionalExpr,
  EnclosedExpr,
  Expression,
  FieldAccessExpr,
  InstanceOfExpr,
  LambdaExpr,
  LiteralExpr,
  MethodCallExpr,
  MethodReferenceExpr,
  NameExpr,
  ObjectCreationExpr,
  PatternExpr,
  SuperExpr,
  SwitchExpr,
  ThisExpr,
  TypeExpr,
  UnaryExpr,
  VariableDeclarationExpr
}
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
  Statement,
  SwitchStmt,
  SynchronizedStmt,
  ThrowStmt,
  TryStmt,
  UnparsableStmt,
  WhileStmt,
  YieldStmt
}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewNamespaceBlock,
  NewNode,
  NewTypeDecl
}
import io.shiftleft.passes.DiffGraph

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.util.{Failure, Success, Try}

class AstCreator(filename: String) {

  import AstCreator._

  val stack: mutable.Stack[NewNode] = mutable.Stack()
  val diffGraph: DiffGraph.Builder  = DiffGraph.newBuilder

  def createAst(parserResult: CompilationUnit): Iterator[DiffGraph] = {
    parserResult.getPackageDeclaration.asScala.foreach { packageDecl =>
      val namespaceBlock = addNamespaceBlock(packageDecl, filename)
      stack.push(namespaceBlock)
    }
    withOrder(parserResult.getTypes) { (typ, order) =>
      stack.push(addTypeDeclNode(typ, order))
      withOrder(typ.getMethods) { (m, order) => addMethod(m, typ, order) }
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
    val name         = packageName.split("\\.").lastOption.getOrElse("")
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
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
    withOrder(methodDeclaration.getParameters) { (p, order) =>
      addParameter(p, order)
    }
    val methodReturnNode = addMethodReturnNode(methodDeclaration)
    diffGraph.addEdge(methodNode, methodReturnNode, EdgeTypes.AST)
    addMethodBody(methodDeclaration)
    stack.pop()
  }

  private def addMethodBody(methodDeclaration: MethodDeclaration) = {
    methodDeclaration.getBody.asScala.foreach { body =>
      addStatements(body.getStatements)
    }
  }

  private def addStatements(nodeList: NodeList[Statement]) = {
    withOrder(nodeList) { (x, order) =>
      addStatement(x, order)
    }
  }

  def addBreakStatement(stmt: BreakStmt, order: Int): Unit = {
    val node = NewControlStructure(
      controlStructureType = ControlStructureTypes.BREAK,
      lineNumber = line(stmt),
      columnNumber = column(stmt),
      code = stmt.toString
    )
    diffGraph.addNode(node)
  }

  def addSwitchStatement(stmt: SwitchStmt, order: Int): Unit = {
    stmt.getEntries.asScala.foreach { entry =>
      addStatements(entry.getStatements)
    }
  }

  private def addStatement(statement: Statement, order: Int): Unit = {
    statement match {
      case x: AssertStmt                        =>
      case x: BlockStmt                         => addBlockStatement(x, order)
      case x: BreakStmt                         => addBreakStatement(x, order)
      case x: ContinueStmt                      =>
      case x: EmptyStmt                         =>
      case x: ExplicitConstructorInvocationStmt =>
      case x: ExpressionStmt                    => addExpression(x.getExpression, order)
      case x: ForEachStmt                       =>
      case x: ForStmt                           =>
      case x: IfStmt                            =>
      case x: LabeledStmt                       =>
      case x: LocalClassDeclarationStmt         =>
      case x: LocalRecordDeclarationStmt        =>
      case x: ReturnStmt                        => addReturnNode(x, order)
      case x: SwitchStmt                        => addSwitchStatement(x, order)
      case x: SynchronizedStmt                  =>
      case x: ThrowStmt                         =>
      case x: TryStmt                           =>
      case x: UnparsableStmt                    =>
      case x: WhileStmt                         =>
      case x: YieldStmt                         =>
      case _                                    =>
    }
  }

  private def addBlockStatement(stmt: BlockStmt, order: Int = 1): Unit = {
    val block = NewBlock(order = order, lineNumber = line(stmt), columnNumber = column(stmt))
    diffGraph.addNode(block)
    withOrder(stmt.getStatements) { (x, order) =>
      addStatement(x, order)
    }
  }

  private def addReturnNode(ret: ReturnStmt, order: Int = 1): Unit = {
    // TODO: Make return node with expression as children
    if (ret.getExpression.isPresent) {
      addExpression(ret.getExpression.get(), order + 1)
    }
  }

  private def addExpression(expression: Expression, order: Int = 1): Unit = {
    expression match {
      case x: AnnotationExpr          =>
      case x: ArrayAccessExpr         =>
      case x: ArrayInitializerExpr    =>
      case x: AssignExpr              =>
      case x: BinaryExpr              =>
      case x: CastExpr                =>
      case x: ClassExpr               =>
      case x: ConditionalExpr         =>
      case x: EnclosedExpr            =>
      case x: FieldAccessExpr         =>
      case x: InstanceOfExpr          =>
      case x: LambdaExpr              =>
      case x: LiteralExpr             =>
      case x: MethodCallExpr          => addMethodCall(x, order)
      case x: MethodReferenceExpr     =>
      case x: NameExpr                =>
      case x: ObjectCreationExpr      =>
      case x: PatternExpr             =>
      case x: SuperExpr               =>
      case x: SwitchExpr              =>
      case x: ThisExpr                =>
      case x: TypeExpr                =>
      case x: UnaryExpr               =>
      case x: VariableDeclarationExpr =>
      case _                          =>
    }
  }

  private def addMethodCall(call: MethodCallExpr, order: Int = 1): NewCall = {
    val callNode = NewCall()
      .name(call.getNameAsString)
      .code(s"${call.getNameAsString}(${call.getArguments.asScala.mkString(", ")})")
      .order(order)
      .argumentIndex(order)
    Try(call.resolve()) match {
      case Success(x) =>
        val signature = s"${x.getReturnType.describe()}(${(for (i <- 0 until x.getNumberOfParams)
          yield x.getParam(i).getType.describe()).mkString(",")})"
        callNode.methodFullName(s"${x.getQualifiedName}:$signature")
        callNode.signature(signature)
      // TODO: Generate AST children here
      case Failure(_) =>
    }
    if (call.getName.getBegin.isPresent) {
      callNode
        .lineNumber(line(call.getName))
        .columnNumber(column(call.getName))
    }
    diffGraph.addNode(callNode)
    callNode
  }

  private def addMethodReturnNode(methodDeclaration: MethodDeclaration) = {
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameters.size + 2)
        .typeFullName(methodDeclaration.getType.resolve().describe())
        .code(methodDeclaration.getTypeAsString)
        .lineNumber(line(methodDeclaration.getType))
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
      .lineNumber(line(methodDeclaration))
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
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
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

object AstCreator {
  def line(node: Node): Option[Integer] = {
    node.getBegin.map(x => Integer.valueOf(x.line)).asScala
  }

  def column(node: Node): Option[Integer] = {
    node.getBegin.map(x => Integer.valueOf(x.column)).asScala
  }

  def withOrder[T <: Node](nodeList: java.util.List[T])(f: (T, Int) => Unit): Unit = {
    nodeList.asScala.zipWithIndex.foreach { case (x, i) =>
      val order = i + 1
      f(x, order)
    }
  }
}
