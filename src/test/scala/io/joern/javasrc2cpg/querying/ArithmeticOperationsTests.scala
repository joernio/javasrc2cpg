package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters}
import org.scalatest.Ignore

@Ignore
class ArithmeticOperationsTests extends JavaSrcCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      | class Foo {
      |   static void main(int argc, char argv) {
      |     int a = 1;
      |     int b = 2.0;
      |     int c = a + b;
      |     int d = c - a;
      |     int e = a * b;
      |     int f = b / a;
      |   }
      | }
      |""".stripMargin

  val vars = Seq(
    ("a", "int"),
    ("b", "int"),
    ("c", "int"),
    ("d", "int"),
    ("e", "int"),
    ("f", "int"),
  )

  "should contain call nodes with <operation>.assignment for all variables" in {
    val assignments = cpg.call(Operators.assignment).map(x => (x.name, x.typeFullName)).l
    assignments.size shouldBe 6
    vars.foreach(x => {
      assignments contains x shouldBe true
    })
  }

  "should contain all local variables for the given procedure" in {
    val variables = cpg.local.l
    variables.foreach(x => {
      vars contains Tuple2(x.name, "int") shouldBe true
      x.typeFullName shouldBe "int"
    })
  }

  "should contain a call node for the addition operator" in {
    val List(op) = cpg.call(Operators.addition).l
    val List(a, b) = op.astOut.l
    a.isInstanceOf[Identifier] shouldBe true
    b.isInstanceOf[Identifier] shouldBe true
    a.asInstanceOf[Identifier].name shouldBe "a"
    b.asInstanceOf[Identifier].name shouldBe "b"
  }

  "should contain a call node for the subtraction operator" in {
    val List(op) = cpg.call(Operators.subtraction).l
    val List(c, a) = op.astOut.l
    c.isInstanceOf[Identifier] shouldBe true
    a.isInstanceOf[Identifier] shouldBe true
    c.asInstanceOf[Identifier].name shouldBe "c"
    a.asInstanceOf[Identifier].name shouldBe "a"
  }

  "should contain a call node for the multiplication operator" in {
    val List(op) = cpg.call(Operators.multiplication).l
    val List(a, b) = op.astOut.l
    a.isInstanceOf[Identifier] shouldBe true
    b.isInstanceOf[Identifier] shouldBe true
    a.asInstanceOf[Identifier].name shouldBe "a"
    b.asInstanceOf[Identifier].name shouldBe "b"
  }

  "should contain a call node for the division operator" in {
    val List(op) = cpg.call(Operators.division).l
    val List(b, a) = op.astOut.l
    a.isInstanceOf[Identifier] shouldBe true
    b.isInstanceOf[Identifier] shouldBe true
    a.asInstanceOf[Identifier].name shouldBe "a"
    b.asInstanceOf[Identifier].name shouldBe "b"
  }
}
