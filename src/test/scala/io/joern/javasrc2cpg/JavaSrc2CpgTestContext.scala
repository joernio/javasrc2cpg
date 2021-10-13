package io.joern.javasrc2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.layers.{LayerCreatorContext, Scpg}

import java.io.{File, PrintWriter}
import java.nio.file.Files

class JavaSrc2CpgTestContext {
  private var code: String = ""
  private var buildResult = Option.empty[Cpg]

  def buildCpg: Cpg = {
    if (buildResult.isEmpty) {
      val javaSrc2Cpg = JavaSrc2Cpg()
      val cpg = javaSrc2Cpg.createCpg(writeCodeToFile(code).getAbsolutePath)
      val context = new LayerCreatorContext(cpg)
      new Scpg().run(context)
      buildResult = Some(cpg)
    }
    buildResult.get
  }

  private def withSource(code: String): JavaSrc2CpgTestContext = {
    this.code = code
    this
  }

  private def writeCodeToFile(code: String): File = {
    val tmpDir = Files.createTempDirectory("javasrc2cpgTest").toFile
    tmpDir.deleteOnExit()
    val codeFile = File.createTempFile("Test", ".java", tmpDir)
    codeFile.deleteOnExit()
    new PrintWriter(codeFile) { write(code); close() }
    tmpDir
  }
}

object JavaSrc2CpgTestContext {
  def buildCpg(code: String): Cpg = {
    new JavaSrc2CpgTestContext()
      .withSource(code)
      .buildCpg
  }
}