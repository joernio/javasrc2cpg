package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class AstTests extends JavaSrcCodeToCpgFixture {

  override val code =
    """
      |package com.foo;
      |public class Test {
      | public static void foo() {
      |    System.out.println("Hello World");
      | }
      |}
      |""".stripMargin

  "foo" in {
    cpg.method.foreach(println)
  }

}
