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
  SwitchEntry,
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
  NewFile,
  NewJumpTarget,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewNamespaceBlock,
  NewNamespaceBlockBuilder,
  NewNode,
  NewTypeDecl
}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.util.{Failure, Success, Try}

case class AstEdge(src: NewNode, dst: NewNode)

object Ast {
  def apply(node: NewNode): Ast = Ast(List(node))
  def apply(): Ast              = new Ast(List())
}

case class Ast(nodes: List[NewNode], edges: List[AstEdge] = List()) {

  def root: Option[NewNode]          = nodes.headOption
  def rightMostLeaf: Option[NewNode] = nodes.lastOption

  /** AST that results when adding `other` as a child to this AST.
    * `other` is connected to this AST's root node.
    */
  def withChild(other: Ast): Ast = {
    Ast(
      nodes ++ other.nodes,
      edges = edges ++ other.edges ++ root.toList.flatMap(r =>
        other.root.toList.map { rc => AstEdge(r, rc) }
      )
    )
  }

  /** AST that results when adding all ASTs in `asts` as children,
    * that is, connecting them to the root node of this AST.
    */
  def withChildren(asts: Seq[Ast]): Ast = {
    asts.headOption match {
      case Some(head) =>
        withChild(head).withChildren(asts.tail)
      case None =>
        this
    }
  }

}

class AstCreator(filename: String) {

  import AstCreator._

  val stack: mutable.Stack[NewNode] = mutable.Stack()
  val diffGraph: DiffGraph.Builder  = DiffGraph.newBuilder

  def createAst(parserResult: CompilationUnit): Iterator[DiffGraph] = {
    storeInDiffGraph(astForFile(parserResult))
    Iterator(diffGraph.build)
  }

  /** Copy nodes/edges of given `AST` into the diff graph
    */
  private def storeInDiffGraph(ast: Ast): Unit = {
    ast.nodes.foreach { node =>
      diffGraph.addNode(node)
    }
    ast.edges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.AST)
    }
  }

  def astForFile(parserResult: CompilationUnit): Ast = {
    Ast(NewFile(name = filename, order = 0))
      .withChild(
        astForPackageDeclaration(parserResult.getPackageDeclaration.asScala)
          .withChildren(withOrder(parserResult.getTypes) { (typ, order) =>
            astForTypeDecl(typ, order)
          })
      )
  }

  def astForTypeDecl(typ: TypeDeclaration[_], order: Int): Ast = {
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
      .order(order)
      .filename(filename)
    Ast(typeDecl).withChildren(
      withOrder(typ.getMethods) { (m, order) => astForMethod(m, typ, order) }
    )
  }

  def astForPackageDeclaration(packageDecl: Option[PackageDeclaration]): Ast = {

    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val namespaceBlock = packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val name        = packageName.split("\\.").lastOption.getOrElse("")
        NewNamespaceBlock()
          .name(name)
          .fullName(packageName)
      case None =>
        NewNamespaceBlock()
          .name(globalNamespaceName)
          .fullName(globalNamespaceName)
    }
    Ast(namespaceBlock.filename(absolutePath).order(1))
  }

  private def astForMethod(
      methodDeclaration: MethodDeclaration,
      typeDecl: TypeDeclaration[_],
      childNum: Int
  ): Ast = {
    val methodNode = createMethodNode(methodDeclaration, typeDecl, childNum)
    val parameterAsts = withOrder(methodDeclaration.getParameters) { (p, order) =>
      astForParameter(p, order)
    }
    val lastOrder = 2 + parameterAsts.size
    Ast(methodNode)
      .withChildren(parameterAsts)
      .withChild(astForMethodReturn(methodDeclaration))
      .withChild(astForMethodBody(methodDeclaration.getBody.asScala, lastOrder))
  }

  private def astForMethodReturn(methodDeclaration: MethodDeclaration): Ast = {
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameters.size + 2)
        .typeFullName(methodDeclaration.getType.resolve().describe())
        .code(methodDeclaration.getTypeAsString)
        .lineNumber(line(methodDeclaration.getType))
    Ast(methodReturnNode)
  }

  private def createMethodNode(
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
    methodNode
  }

  private def astForMethodBody(body: Option[BlockStmt], order: Int): Ast = {
    body match {
      case Some(b) => astForBlockStatement(b, order)
      case None =>
        val blockNode = NewBlock()
        Ast(blockNode)
    }
  }

  private def astForStatement(statement: Statement, order: Int): Ast = {
    statement match {
      case x: AssertStmt                        => Ast()
      case x: BlockStmt                         => astForBlockStatement(x, order)
      case x: BreakStmt                         => astForBreakStatement(x, order)
      case x: ContinueStmt                      => Ast()
      case x: EmptyStmt                         => Ast()
      case x: ExplicitConstructorInvocationStmt => Ast()
      case x: ExpressionStmt                    => astForExpressionStmt(x.getExpression, order)
      case x: ForEachStmt                       => Ast()
      case x: ForStmt                           => Ast()
      case x: IfStmt                            => Ast()
      case x: LabeledStmt                       => Ast()
      case x: LocalClassDeclarationStmt         => Ast()
      case x: LocalRecordDeclarationStmt        => Ast()
      case x: ReturnStmt                        => astForReturnNode(x, order)
      case x: SwitchStmt                        => astForSwitchStatement(x, order)
      case x: SynchronizedStmt                  => Ast()
      case x: ThrowStmt                         => Ast()
      case x: TryStmt                           => Ast()
      case x: UnparsableStmt                    => Ast()
      case x: WhileStmt                         => Ast()
      case x: YieldStmt                         => Ast()
      case _                                    => Ast()
    }
  }

  def astForBreakStatement(stmt: BreakStmt, order: Int): Ast = {
    val node = NewControlStructure(
      controlStructureType = ControlStructureTypes.BREAK,
      lineNumber = line(stmt),
      columnNumber = column(stmt),
      code = stmt.toString,
      order = order
    )
    Ast(node)
  }

  def astForSwitchStatement(stmt: SwitchStmt, order: Int): Ast = {
    val switchNode =
      NewControlStructure(controlStructureType = ControlStructureTypes.SWITCH, order = order)
    val entryAsts = withOrder(stmt.getEntries) { (e, order) => astForSwitchEntry(e, order) }.flatten
    Ast(switchNode).withChildren(entryAsts)
  }

  def astForSwitchEntry(entry: SwitchEntry, order: Int): Seq[Ast] = {
    val labelNodes = withOrder(entry.getLabels) { (x, o) =>
      NewJumpTarget(name = x.toString, order = o + order)
    }
    val statementAsts = withOrder(entry.getStatements) { (s, o) =>
      astForStatement(s, order + o + labelNodes.size)
    }
    labelNodes.map(x => Ast(x)) ++ statementAsts
  }

  private def astForBlockStatement(stmt: BlockStmt, order: Int): Ast = {
    val block = NewBlock(order = order, lineNumber = line(stmt), columnNumber = column(stmt))
    Ast(block).withChildren(
      withOrder(stmt.getStatements) { (x, order) =>
        astForStatement(x, order)
      }
    )
  }

  private def astForReturnNode(ret: ReturnStmt, order: Int): Ast = {
    // TODO: Make return node with expression as children
    if (ret.getExpression.isPresent) {
      astForExpressionStmt(ret.getExpression.get(), order + 1)
    } else {
      Ast()
    }
  }

  private def astForExpressionStmt(expression: Expression, order: Int = 1): Ast = {
    expression match {
      case x: AnnotationExpr          => Ast()
      case x: ArrayAccessExpr         => Ast()
      case x: ArrayInitializerExpr    => Ast()
      case x: AssignExpr              => Ast()
      case x: BinaryExpr              => Ast()
      case x: CastExpr                => Ast()
      case x: ClassExpr               => Ast()
      case x: ConditionalExpr         => Ast()
      case x: EnclosedExpr            => Ast()
      case x: FieldAccessExpr         => Ast()
      case x: InstanceOfExpr          => Ast()
      case x: LambdaExpr              => Ast()
      case x: LiteralExpr             => Ast()
      case x: MethodCallExpr          => astForMethodCall(x, order)
      case x: MethodReferenceExpr     => Ast()
      case x: NameExpr                => Ast()
      case x: ObjectCreationExpr      => Ast()
      case x: PatternExpr             => Ast()
      case x: SuperExpr               => Ast()
      case x: SwitchExpr              => Ast()
      case x: ThisExpr                => Ast()
      case x: TypeExpr                => Ast()
      case x: UnaryExpr               => Ast()
      case x: VariableDeclarationExpr => Ast()
      case _                          => Ast()
    }
  }

  private def astForMethodCall(call: MethodCallExpr, order: Int = 1): Ast = {

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
    Ast(callNode)
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .typeFullName(parameter.getType.resolve().describe())
      .order(childNum)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
    Ast(parameterNode)
  }

  private def methodFullName(
      typeDecl: TypeDeclaration[_],
      methodDeclaration: MethodDeclaration
  ): String = {
    val typeName   = typeDecl.getFullyQualifiedName.asScala.getOrElse("")
    val returnType = methodDeclaration.getTypeAsString
    val methodName = methodDeclaration.getNameAsString
    s"$typeName.$methodName:$returnType${paramListSignature(methodDeclaration)}"
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

  def withOrder[T <: Node, X](nodeList: java.util.List[T])(f: (T, Int) => X): Seq[X] = {
    nodeList.asScala.zipWithIndex.map { case (x, i) =>
      f(x, i + 1)
    }.toSeq
  }
}
