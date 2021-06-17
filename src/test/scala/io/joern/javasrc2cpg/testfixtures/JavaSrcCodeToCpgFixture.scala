package io.joern.javasrc2cpg.testfixtures

import io.joern.javascr2cpg.JavaSrc2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}

import java.io.File

class JavaSrcFrontend extends LanguageFrontend {
  override val fileSuffix: String = ".java"

  override def execute(sourceCodeFile: File): Cpg = {
    new JavaSrc2Cpg().createCpg(sourceCodeFile.getAbsolutePath, Set(fileSuffix))
  }
}

class JavaSrcCodeToCpgFixture extends CodeToCpgFixture(new JavaSrcFrontend) {}