package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, ControlStructure, Identifier, Local}
import io.shiftleft.semanticcpg.language._

class ControlStructureTempTests extends JavaSrcCodeToCpgFixture {

  override val code =
    """
      |class Foo {
      |
      |int baz(Iterable<Integer> xs) {
      |  int sum = 0;
      |  for( Integer x : xs) {
      |    sum += x;
      |  }
      |  return sum;
      |}
      |}
      |""".stripMargin

  "should parse a `foreach` loop as a for" in {
    val List(forLoop: ControlStructure) = cpg.method.name("baz").forBlock.l
    forLoop.controlStructureType shouldBe "FOR"
    val List(iterator: Identifier, variable: Local, body: Block) = forLoop.astChildren.l

    iterator.name shouldBe "xs"
    iterator.typeFullName shouldBe "java.lang.Iterable<java.lang.Integer>"
    variable.name shouldBe "x"
    variable.typeFullName shouldBe "java.lang.Integer"

    body.astChildren.head.code shouldBe "sum += x"
  }
}
