package io.joern.javasrc2cpg.standard

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

class TypeDeclTests extends JavaSrcCodeToCpgFixture {

  override val code =
    """
      |package a.b.c;
      | class Foo extends Bar {
      |   char x;
      |   int y;
      |   int method () { return 0; }
      | }
      |
      |class Bar {}
      |
    """.stripMargin

  "should contain a type decl for `Foo` with correct fields" in {
    val List(x) = cpg.typeDecl("a.b.c.Foo").l
    x.name shouldBe "Foo"
    x.fullName shouldBe "a.b.c.Foo"
    x.isExternal shouldBe false
    x.inheritsFromTypeFullName shouldBe List("a.b.c.Bar")
    x.aliasTypeFullName shouldBe None
    x.order shouldBe 1
    x.filename.startsWith("/") shouldBe true
    x.filename.endsWith(".java") shouldBe true
  }

}
