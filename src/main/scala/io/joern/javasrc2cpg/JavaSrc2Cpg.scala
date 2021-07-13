package io.joern.javasrc2cpg

import io.joern.javasrc2cpg.passes.AstCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.passes.FileCreationPass
import io.shiftleft.semanticcpg.passes.containsedges.ContainsEdgePass
import io.shiftleft.semanticcpg.passes.linking.linker.Linker
import io.shiftleft.semanticcpg.passes.metadata.MetaDataPass
import io.shiftleft.semanticcpg.passes.namespacecreator.NamespaceCreator
import io.shiftleft.semanticcpg.passes.typenodes.{TypeDeclStubCreator, TypeNodePass}
import io.shiftleft.x2cpg.SourceFiles
import io.shiftleft.x2cpg.X2Cpg.newEmptyCpg

import scala.jdk.CollectionConverters.EnumerationHasAsScala

object JavaSrc2Cpg {
  val language = "JAVAPARSER"
}

class JavaSrc2Cpg {

  import JavaSrc2Cpg._

  /** Create CPG for Java source code at `sourceCodePath` and store the
    * CPG at `outputPath`. If `outputPath` is `None`, the CPG is created
    * in-memory.
    */
  def createCpg(
      sourceCodePath: String,
      outputPath: Option[String] = None
  ): Cpg = {

    val cpg             = newEmptyCpg(outputPath)
    val metaDataKeyPool = new IntervalKeyPool(1, 100)
    val typesKeyPool    = new IntervalKeyPool(100, 1000100)
    val methodKeyPool   = new IntervalKeyPool(first = 1000100, last = Long.MaxValue)

    new MetaDataPass(cpg, language, Some(metaDataKeyPool)).createAndApply()

    val sourceFileExtensions = Set(".java")
    val sourceFileNames      = SourceFiles.determine(Set(sourceCodePath), sourceFileExtensions)
    val astCreator           = new AstCreationPass(sourceCodePath, sourceFileNames, cpg, methodKeyPool)
    astCreator.createAndApply()

    new NamespaceCreator(cpg).createAndApply()
    new FileCreationPass(cpg).createAndApply()

    new TypeNodePass(astCreator.global.usedTypes.keys().asScala.toList, cpg, Some(typesKeyPool))
      .createAndApply()
    new TypeDeclStubCreator(cpg).createAndApply()

    new ContainsEdgePass(cpg).createAndApply()
    new Linker(cpg).createAndApply()
    cpg
  }

}
