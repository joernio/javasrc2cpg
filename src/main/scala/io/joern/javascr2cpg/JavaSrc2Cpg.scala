package io.joern.javascr2cpg

import io.joern.javascr2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.FileCreationPass
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass
import io.shiftleft.semanticcpg.passes.namespacecreator.NamespaceCreator
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg

class JavaSrc2Cpg {

  def createCpg(
      sourceCodePaths: Set[String],
      outputPath: Option[String] = None
  ): Cpg = {
    val sourceFileExtensions = Set(".java")
    val sourceFileNames      = SourceFiles.determine(sourceCodePaths, sourceFileExtensions)
    val cpg                  = newEmptyCpg(outputPath)
    val metaDataKeyPool      = new IntervalKeyPool(1, 100)
    val methodKeyPool        = new IntervalKeyPool(1000100, Long.MaxValue)
    val metaDataPass         = new MetaDataPass(cpg, "JAVAPARSER", Some(metaDataKeyPool))
    metaDataPass.createAndApply()

    sourceCodePaths.toList.sorted.foreach { sourceCodePath =>
      val astCreator = new AstCreationPass(sourceCodePath, sourceFileNames, cpg, methodKeyPool)
      astCreator.createAndApply()
    }
    new NamespaceCreator(cpg).createAndApply()
    new FileCreationPass(cpg).createAndApply()
    cpg
  }

}
