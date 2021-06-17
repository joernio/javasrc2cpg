package io.joern.javascr2cpg

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg

class JavaSrc2Cpg {

  def createCpg(
      sourceCodePath: String,
      sourceFileExtensions: Set[String],
      outputPath: Option[String] = None
  ): Cpg = {
    val sourceFileNames = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
    val cpg             = newEmptyCpg(outputPath)
    // Run passes
    // ...
    cpg
  }

}
