package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class AstTests extends JavaSrcCodeToCpgFixture {

  override val code =
    """
      |int foo() {
      |  return 1;
      |}
      |""".stripMargin

  "foo" in {
    cpg.method.foreach(println)
  }

}
