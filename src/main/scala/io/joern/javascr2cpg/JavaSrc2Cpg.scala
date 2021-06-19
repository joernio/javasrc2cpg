package io.joern.javascr2cpg

import io.joern.javascr2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass
import io.shiftleft.semanticcpg.passes.namespacecreator.NamespaceCreator
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

    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val methodKeyPool   = new IntervalKeyPool(1000100, Long.MaxValue)
    val metaDataPass    = new MetaDataPass(cpg, "JAVAPARSER", Some(metaDataKeyPool))
    val astCreator      = new AstCreationPass(sourceCodePath, sourceFileNames, cpg, methodKeyPool)
    metaDataPass.createAndApply()
    astCreator.createAndApply()

    new NamespaceCreator(cpg).createAndApply()

    cpg
  }

}
