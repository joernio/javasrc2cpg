package io.joern.javascr2cpg

import io.joern.javascr2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.FileCreationPass
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass
import io.shiftleft.semanticcpg.passes.namespacecreator.NamespaceCreator
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg

object JavaSrc2Cpg {
  val language = "JAVAPARSER"
}

class JavaSrc2Cpg {

  import JavaSrc2Cpg._

  def createCpg(
      sourceCodePath: String,
      outputPath: Option[String] = None
  ): Cpg = {

    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val methodKeyPool   = new IntervalKeyPool(first = 1000100, last = Long.MaxValue)

    val cpg          = newEmptyCpg(outputPath)
    val metaDataPass = new MetaDataPass(cpg, language, Some(metaDataKeyPool))
    metaDataPass.createAndApply()
    val sourceFileExtensions = Set(".java")
    val sourceFileNames      = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
    val astCreator           = new AstCreationPass(sourceCodePath, sourceFileNames, cpg, methodKeyPool)
    astCreator.createAndApply()
    new NamespaceCreator(cpg).createAndApply()
    new FileCreationPass(cpg).createAndApply()
    cpg
  }

}
