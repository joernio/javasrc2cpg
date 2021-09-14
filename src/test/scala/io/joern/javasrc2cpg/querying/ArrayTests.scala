package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve}
import io.shiftleft.semanticcpg.language._

class ArrayTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      |class Foo {
      |  public void foo() {
      |    int[] x = {0, 1, 2};
      |  }
      |
      |  public void bar() {
      |    int[][] x = new int[5][2];
      |  }
      |}
      |""".stripMargin

  "should initialize array with constant initialization expression" in {
    def m = cpg.method(".*foo.*")
    val List(assignment) = m.assignments.l

    val arg1 = assignment.argument(1)
    val arg2 = assignment.argument(2)

    arg1 shouldBe a[Identifier]
    arg1.asInstanceOf[Identifier].code shouldBe "x"
    arg1.asInstanceOf[Identifier].typeFullName shouldBe "int[]"

    arg2 shouldBe a[Call]
    arg2.code shouldBe "{ 0, 1, 2 }"
    arg2.astChildren.zipWithIndex.foreach { case (arg, idx) =>
      arg shouldBe a[Literal]
      arg.code shouldBe idx.toString
    }
  }

  "should initialize an array with empty initialization expression" in {
    def m = cpg.method(".*bar.*")
    val List(assignment) = m.assignments.l

    val arg1 = assignment.argument(1)
    val arg2 = assignment.argument(2)

    arg1 shouldBe a[Identifier]
    arg1.asInstanceOf[Identifier].typeFullName shouldBe "int[][]"

    arg2 shouldBe a[Call]
    arg2.code shouldBe "new int[5][2]"
    val lvl1 = arg2.asInstanceOf[Call].argument(1)
    val lvl2 = arg2.asInstanceOf[Call].argument(2)
    lvl1.code shouldBe "5"
    lvl2.code shouldBe "2"
  }
}