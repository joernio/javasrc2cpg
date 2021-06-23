package io.joern.javascr2cpg

import io.shiftleft.x2cpg.{X2Cpg, X2CpgConfig}
import scopt.OParser

final case class Config(
    inputPaths: Set[String] = Set.empty,
    outputPath: String = X2CpgConfig.defaultOutputPath
) extends X2CpgConfig[Config] {

  override def withAdditionalInputPath(inputPath: String): Config =
    copy(inputPaths = inputPaths + inputPath)
  override def withOutputPath(x: String): Config = copy(outputPath = x)
}

object Main extends App {

  val frontendSpecificOptions = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("javasrc2cpg")
    )
  }

  X2Cpg.parseCommandLine(args, frontendSpecificOptions, Config()) match {
    case Some(config) =>
      println("Run frontend")
    case None =>
      System.exit(1)
  }

}
