name := "ghidra2cpg"
organization := "io.joern"

version := "0.1"
scalaVersion := "2.13.4"

val cpgVersion       = "1.3.201"
val scalatestVersion = "3.1.1"

fork := true
resolvers += Resolver.mavenLocal
trapExit := false

libraryDependencies ++= Seq(
  "io.shiftleft" % "ghidra" % "9.2_PUBLIC_20201113" ,
  "io.shiftleft"  %% "codepropertygraph"        % cpgVersion,
  "io.shiftleft"  %% "codepropertygraph-protos" % cpgVersion,
  "io.shiftleft"  %% "semanticcpg"              % cpgVersion,
  "io.shiftleft"  %% "dataflowengineoss"        % cpgVersion,
  "io.shiftleft"  %% "semanticcpg-tests"        % cpgVersion       % Test classifier "tests",
  "commons-io"     % "commons-io"               % "2.7",
  "org.scalatest" %% "scalatest"                % scalatestVersion % Test
)

enablePlugins(JavaAppPackaging)

Global / onChangedBuildSource := ReloadOnSourceChanges
