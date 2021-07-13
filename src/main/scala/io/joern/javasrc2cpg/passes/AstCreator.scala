package io.joern.javasrc2cpg.passes

import com.github.javaparser.ast.{CompilationUnit, Node, PackageDeclaration}
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
  DoubleLiteralExpr,
  EnclosedExpr,
  Expression,
  FieldAccessExpr,
  InstanceOfExpr,
  IntegerLiteralExpr,
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
  DoStmt,
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
import com.github.javaparser.resolution.declarations.ResolvedMethodDeclaration
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewCall,
  NewControlStructure,
  NewFile,
  NewIdentifier,
  NewIdentifierBuilder,
  NewJumpTarget,
  NewLiteral,
  NewLocal,
  NewMember,
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
import io.shiftleft.x2cpg.Ast

import scala.jdk.CollectionConverters._
import scala.collection.mutable
import scala.compat.java8.OptionConverters.RichOptionalGeneric
import scala.util.{Failure, Success, Try}

class AstCreator(filename: String, global: Global) {

  import AstCreator._

  val stack: mutable.Stack[NewNode] = mutable.Stack()
  val diffGraph: DiffGraph.Builder  = DiffGraph.newBuilder

  /** Add `typeName` to a global map and return it. The
    * map is later passed to a pass that creates TYPE
    * nodes for each key in the map.
    */
  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  /** Entry point of AST creation. Translates a compilation
    * unit created by JavaParser into a DiffGraph containing
    * the corresponding CPG AST.
    */
  def createAst(parserResult: CompilationUnit): Iterator[DiffGraph] = {
    storeInDiffGraph(astForCompilationUnit(parserResult))
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
    ast.conditionEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.CONDITION)
    }
    ast.argEdges.foreach { edge =>
      diffGraph.addEdge(edge.src, edge.dst, EdgeTypes.ARGUMENT)
    }
  }

  /** Translate compilation unit into AST
    */
  private def astForCompilationUnit(compilationUnit: CompilationUnit): Ast = {
    val ast = astForPackageDeclaration(compilationUnit.getPackageDeclaration.asScala)
    val namespaceBlockFullName =
      ast.root.collect { case x: NewNamespaceBlock => x.fullName }.getOrElse("none")
    ast
      .withChildren(withOrder(compilationUnit.getTypes) { (typ, order) =>
        astForTypeDecl(typ, order, namespaceBlockFullName)
      })
  }

  /** Translate package declaration into AST consisting of
    * a corresponding namespace block.
    */
  private def astForPackageDeclaration(packageDecl: Option[PackageDeclaration]): Ast = {
    val absolutePath = new java.io.File(filename).toPath.toAbsolutePath.normalize().toString
    val namespaceBlock = packageDecl match {
      case Some(decl) =>
        val packageName = decl.getName.toString
        val name        = packageName.split("\\.").lastOption.getOrElse("")
        NewNamespaceBlock()
          .name(name)
          .fullName(packageName)
      case None =>
        createGlobalNamespaceBlock
    }
    Ast(namespaceBlock.filename(absolutePath).order(1))
  }

  private def createGlobalNamespaceBlock: NewNamespaceBlockBuilder =
    NewNamespaceBlock()
      .name(globalNamespaceName)
      .fullName(globalNamespaceName)

  private def astForTypeDecl(
      typ: TypeDeclaration[_],
      order: Int,
      namespaceBlockFullName: String
  ): Ast = {
    val baseTypeFullNames = typ
      .asClassOrInterfaceDeclaration()
      .getExtendedTypes
      .asScala
      .map(x => registerType(x.resolve().getQualifiedName))
      .toList

    val typeDecl = NewTypeDecl()
      .name(typ.getNameAsString)
      .fullName(typ.getFullyQualifiedName.asScala.getOrElse(""))
      .inheritsFromTypeFullName(baseTypeFullNames)
      .order(order)
      .filename(filename)
      .code(typ.getNameAsString)
      .astParentType("NAMESPACE_BLOCK")
      .astParentFullName(namespaceBlockFullName)

    val methodAsts = withOrder(typ.getMethods) { (m, order) => astForMethod(m, typ, order) }

    val memberAsts = typ.getMembers.asScala
      .filter(_.isFieldDeclaration)
      .flatMap { m =>
        val fieldDeclaration = m.asFieldDeclaration()
        fieldDeclaration.getVariables.asScala
      }
      .zipWithIndex
      .map { case (v, i) =>
        val typeFullName = v.getType.resolve().describe()
        val name         = v.getName.toString
        Ast(
          NewMember()
            .name(name)
            .typeFullName(typeFullName)
            .order(methodAsts.size + i + 1)
            .code(s"$typeFullName $name")
        )
      }
      .toList

    Ast(typeDecl)
      .withChildren(memberAsts)
      .withChildren(methodAsts)
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
    val typeFullName = registerType(methodDeclaration.getType.resolve().describe())
    val methodReturnNode =
      NewMethodReturn()
        .order(methodDeclaration.getParameters.size + 2)
        .typeFullName(typeFullName)
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

  def astsForLabeledStatement(stmt: LabeledStmt, order: Int): Seq[Ast] = {
    val jumpTargetAst = Ast(NewJumpTarget(name = stmt.getLabel.toString, order = order))
    val stmtAst       = astsForStatement(stmt.getStatement, order = order + 1)
    Seq(jumpTargetAst) ++ stmtAst
  }

  def astForTry(stmt: TryStmt, order: Int): Ast = {
    val tryNode = NewControlStructure(
      controlStructureType = ControlStructureTypes.TRY,
      code = "try",
      order = order
    )
    Ast(tryNode)
  }

  private def astsForStatement(statement: Statement, order: Int): Seq[Ast] = {
    statement match {
      case x: AssertStmt                        => Seq() // TODO: translate to Call
      case x: BlockStmt                         => Seq(astForBlockStatement(x, order))
      case x: BreakStmt                         => Seq(astForBreakStatement(x, order))
      case x: ContinueStmt                      => Seq(astForContinueStatement(x, order))
      case x: DoStmt                            => Seq(astForDo(x, order))
      case x: EmptyStmt                         => Seq()
      case x: ExplicitConstructorInvocationStmt => Seq() // TODO: translate to Call
      case x: ExpressionStmt                    => astsForExpression(x.getExpression, order)
      case x: ForEachStmt                       => Seq() // TODO: translate to For
      case x: ForStmt                           => Seq(astForFor(x, order))
      case x: IfStmt                            => Seq(astForIf(x, order))
      case x: LabeledStmt                       => astsForLabeledStatement(x, order)
      case x: LocalClassDeclarationStmt         => Seq()
      case x: LocalRecordDeclarationStmt        => Seq()
      case x: ReturnStmt                        => astsForReturnNode(x, order)
      case x: SwitchStmt                        => Seq(astForSwitchStatement(x, order))
      case x: SynchronizedStmt                  => Seq()
      case x: ThrowStmt                         => Seq()
      case x: TryStmt                           => Seq(astForTry(x, order))
      case x: UnparsableStmt                    => Seq()
      case x: WhileStmt                         => Seq(astForWhile(x, order))
      case x: YieldStmt                         => Seq()
      case _                                    => Seq()
    }
  }

  def astForIf(stmt: IfStmt, order: Int): Ast = {
    val ifNode       = NewControlStructure(controlStructureType = ControlStructureTypes.IF, order = order)
    val conditionAst = astsForExpression(stmt.getCondition, order = 0).headOption.getOrElse(Ast())
    val stmtAsts     = astsForStatement(stmt.getThenStmt, order = 1)

    val ast = Ast(ifNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)

    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(ifNode, r)
      case None =>
        ast
    }
  }

  def astForWhile(stmt: WhileStmt, order: Int): Ast = {
    val whileNode =
      NewControlStructure(controlStructureType = ControlStructureTypes.WHILE, order = order)
    val conditionAst = astsForExpression(stmt.getCondition, order = 0).headOption.getOrElse(Ast())
    val stmtAsts     = astsForStatement(stmt.getBody, order = 1)
    val ast = Ast(whileNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(whileNode, r)
      case None =>
        ast
    }
  }

  def astForDo(stmt: DoStmt, order: Int): Ast = {
    val doNode =
      NewControlStructure(controlStructureType = ControlStructureTypes.DO, order = order)
    val conditionAst = astsForExpression(stmt.getCondition, order = 0).headOption.getOrElse(Ast())
    val stmtAsts     = astsForStatement(stmt.getBody, order = 1)
    val ast = Ast(doNode)
      .withChild(conditionAst)
      .withChildren(stmtAsts)
    conditionAst.root match {
      case Some(r) =>
        ast.withConditionEdge(doNode, r)
      case None =>
        ast
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

  def astForContinueStatement(stmt: ContinueStmt, order: Int): Ast = {
    val node = NewControlStructure(
      controlStructureType = ControlStructureTypes.CONTINUE,
      lineNumber = line(stmt),
      columnNumber = column(stmt),
      code = stmt.toString,
      order = order
    )
    Ast(node)
  }

  def astForFor(stmt: ForStmt, order: Int): Ast = {
    val forNode =
      NewControlStructure(controlStructureType = ControlStructureTypes.FOR, order = order)
    val initAsts = withOrder(stmt.getInitialization) { (s, o) =>
      astsForExpression(s, o)
    }.flatten

    val compareAst = stmt.getCompare.asScala.toList
      .flatMap(x => astsForExpression(x, order + initAsts.size + 1))
      .headOption
    val updateAsts = withOrder(stmt.getUpdate) { (s, o) =>
      astsForExpression(s, o + initAsts.size + compareAst.size)
    }.flatten

    val stmtAst =
      astsForStatement(stmt.getBody, initAsts.size + compareAst.size + updateAsts.size + 1)

    val ast = Ast(forNode)
      .withChildren(initAsts)
      .withChildren(compareAst.toList)
      .withChildren(updateAsts)
      .withChildren(stmtAst)

    compareAst.flatMap(_.root) match {
      case Some(c) =>
        ast.withConditionEdge(forNode, c)
      case None => ast
    }
  }

  def astForSwitchStatement(stmt: SwitchStmt, order: Int): Ast = {
    val switchNode =
      NewControlStructure(
        controlStructureType = ControlStructureTypes.SWITCH,
        order = order,
        code = s"switch(${stmt.getSelector.toString})"
      )
    val entryAsts = withOrder(stmt.getEntries) { (e, order) => astForSwitchEntry(e, order) }.flatten
    Ast(switchNode).withChildren(entryAsts)
  }

  def astForSwitchEntry(entry: SwitchEntry, order: Int): Seq[Ast] = {
    val labelNodes = withOrder(entry.getLabels) { (x, o) =>
      NewJumpTarget(name = x.toString, order = o + order)
    }
    val statementAsts = withOrder(entry.getStatements) { (s, o) =>
      astsForStatement(s, order + o + labelNodes.size)
    }.flatten
    labelNodes.map(x => Ast(x)) ++ statementAsts
  }

  private def astForBlockStatement(stmt: BlockStmt, order: Int): Ast = {
    val block = NewBlock(order = order, lineNumber = line(stmt), columnNumber = column(stmt))
    Ast(block).withChildren(
      withOrder(stmt.getStatements) { (x, order) =>
        astsForStatement(x, order)
      }.flatten
    )
  }

  private def astsForReturnNode(ret: ReturnStmt, order: Int): Seq[Ast] = {
    // TODO: Make return node with expression as children
    if (ret.getExpression.isPresent) {
      astsForExpression(ret.getExpression.get(), order + 1)
    } else {
      Seq()
    }
  }

  def astForBinaryExpr(stmt: BinaryExpr, order: Int): Ast = {
    val operatorName = stmt.getOperator match {
      case BinaryExpr.Operator.OR                   => Operators.logicalOr
      case BinaryExpr.Operator.AND                  => Operators.logicalAnd
      case BinaryExpr.Operator.BINARY_OR            => Operators.or
      case BinaryExpr.Operator.BINARY_AND           => Operators.and
      case BinaryExpr.Operator.DIVIDE               => Operators.division
      case BinaryExpr.Operator.EQUALS               => Operators.equals
      case BinaryExpr.Operator.GREATER              => Operators.greaterThan
      case BinaryExpr.Operator.GREATER_EQUALS       => Operators.greaterEqualsThan
      case BinaryExpr.Operator.LESS                 => Operators.lessThan
      case BinaryExpr.Operator.LESS_EQUALS          => Operators.lessEqualsThan
      case BinaryExpr.Operator.LEFT_SHIFT           => Operators.shiftLeft
      case BinaryExpr.Operator.SIGNED_RIGHT_SHIFT   => Operators.logicalShiftRight
      case BinaryExpr.Operator.UNSIGNED_RIGHT_SHIFT => Operators.arithmeticShiftRight
      case BinaryExpr.Operator.XOR                  => Operators.xor
      case BinaryExpr.Operator.NOT_EQUALS           => Operators.notEquals
      case BinaryExpr.Operator.PLUS                 => Operators.addition
      case BinaryExpr.Operator.MINUS                => Operators.subtraction
      case BinaryExpr.Operator.MULTIPLY             => Operators.multiplication
      case BinaryExpr.Operator.REMAINDER            => Operators.modulo
      case _                                        => ""
    }

    val callNode = NewCall()
      .name(operatorName)
      .methodFullName(operatorName)
      .code(stmt.toString)
      .argumentIndex(order)
      .order(order)

    val args = astsForExpression(stmt.getLeft, 0) ++ astsForExpression(stmt.getRight, 1)
    callAst(callNode, args)
  }

  def astForVariableDecl(x: VariableDeclarationExpr, order: Int): Seq[Ast] = {
    x.getVariables.asScala.toList.flatMap { v =>
      val name         = v.getName.toString
      val code         = v.getType + " " + v.getName.toString
      val typeFullName = registerType(v.getType.resolve().describe())

      val initializerAst = v.getInitializer.asScala.zipWithIndex.map { case (initializer, i) =>
        val code                    = s"$name = ${initializer.toString}"
        val initializerTypeFullName = registerType(initializer.calculateResolvedType().describe())
        val identifier = NewIdentifier()
          .name(name)
          .order(1)
          .argumentIndex(1)
          .code(name)
          .typeFullName(initializerTypeFullName)
        val assignment = NewCall()
          .name(Operators.assignment)
          .code(code)
          .order(i + 1)
          .argumentIndex(i + 1)
          .typeFullName(typeFullName)
          .build

        val initAsts = astsForExpression(initializer, 2)
        callAst(assignment, Seq(Ast(identifier)) ++ initAsts)
      }

      Seq(
        Ast(
          NewLocal().name(name).code(code).typeFullName(typeFullName).order(order)
        )
      ) ++ initializerAst.toList
    }
  }

  def callAst(rootNode: NewNode, args: Seq[Ast]): Ast = {
    Ast(rootNode)
      .withChildren(args)
      .withArgEdges(rootNode, args.flatMap(_.root))
  }

  def astForIntegerLiteral(x: IntegerLiteralExpr, order: Int): Ast = {
    registerType("int")
    Ast(NewLiteral().order(order).argumentIndex(order).code(x.toString).typeFullName("int"))
  }

  def astForDoubleLiteral(x: DoubleLiteralExpr, order: Int): Ast = {
    registerType("double")
    Ast(NewLiteral().order(order).argumentIndex(order).code(x.toString).typeFullName("double"))
  }

  def astForNameExpr(x: NameExpr, order: Int): Ast = {
    val name         = x.getName.toString
    val typeFullName = registerType(x.resolve().getType.describe())
    Ast(
      NewIdentifier()
        .name(name)
        .order(order)
        .argumentIndex(order)
        .code(name)
        .typeFullName(typeFullName)
    )
  }

  private def astsForExpression(expression: Expression, order: Int): Seq[Ast] = {
    expression match {
      case x: AnnotationExpr          => Seq()
      case x: ArrayAccessExpr         => Seq()
      case x: ArrayInitializerExpr    => Seq()
      case x: AssignExpr              => Seq()
      case x: BinaryExpr              => Seq(astForBinaryExpr(x, order))
      case x: CastExpr                => Seq()
      case x: ClassExpr               => Seq()
      case x: ConditionalExpr         => Seq()
      case x: DoubleLiteralExpr       => Seq(astForDoubleLiteral(x, order))
      case x: EnclosedExpr            => Seq()
      case x: FieldAccessExpr         => Seq()
      case x: IntegerLiteralExpr      => Seq(astForIntegerLiteral(x, order))
      case x: InstanceOfExpr          => Seq()
      case x: LambdaExpr              => Seq()
      case x: LiteralExpr             => Seq()
      case x: MethodCallExpr          => Seq(astForMethodCall(x, order))
      case x: MethodReferenceExpr     => Seq()
      case x: NameExpr                => Seq(astForNameExpr(x, order))
      case x: ObjectCreationExpr      => Seq()
      case x: PatternExpr             => Seq()
      case x: SuperExpr               => Seq()
      case x: SwitchExpr              => Seq()
      case x: ThisExpr                => Seq()
      case x: TypeExpr                => Seq()
      case x: UnaryExpr               => Seq()
      case x: VariableDeclarationExpr => astForVariableDecl(x, order)
      case _                          => Seq()
    }
  }

  private def createCallNode(
      call: MethodCallExpr,
      resolvedDecl: Try[ResolvedMethodDeclaration],
      order: Int
  ) = {
    val callNode = NewCall()
      .name(call.getNameAsString)
      .code(s"${call.getNameAsString}(${call.getArguments.asScala.mkString(", ")})")
      .order(order)
      .argumentIndex(order)
    resolvedDecl match {
      case Success(resolved) =>
        val signature =
          s"${resolved.getReturnType.describe()}(${(for (i <- 0 until resolved.getNumberOfParams)
            yield resolved.getParam(i).getType.describe()).mkString(",")})"
        callNode.methodFullName(s"${resolved.getQualifiedName}:$signature")
        callNode.signature(signature)
      case Failure(exception) =>

    }
    if (call.getName.getBegin.isPresent) {
      callNode
        .lineNumber(line(call.getName))
        .columnNumber(column(call.getName))
    }
    callNode
  }

  private def createThisNode(
      resolvedDecl: Try[ResolvedMethodDeclaration]
  ): Option[NewIdentifierBuilder] = {
    resolvedDecl.toOption
      .filterNot(_.isStatic)
      .map { resolved =>
        NewIdentifier()
          .name("this")
          .code("this")
          .typeFullName(resolved.declaringType().getQualifiedName)
          .order(0)
          .argumentIndex(0)
      }
  }

  private def astForMethodCall(call: MethodCallExpr, order: Int = 1): Ast = {

    val resolvedDecl = Try(call.resolve())
    val callNode     = createCallNode(call, resolvedDecl, order)
    val thisAsts     = createThisNode(resolvedDecl).map(x => Ast(x.build)).toList

    val argAsts = withOrder(call.getArguments) { case (arg, order) =>
      // FIXME: There's an implicit assumption here that each call to
      // astsForExpression only returns a single tree.
      astsForExpression(arg, order)
    }.flatten

    Ast(callNode)
      .withChildren(thisAsts)
      .withChildren(argAsts)
      .withArgEdges(callNode, thisAsts.flatMap(_.root))
      .withArgEdges(callNode, argAsts.flatMap(_.root))
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val typeFullName = registerType(parameter.getType.resolve().describe())
    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .typeFullName(typeFullName)
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
