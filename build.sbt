ThisBuild / name := "javasrc2cpg"
ThisBuild / organization := "io.joern"
publish / skip := true

ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "2.13.6"

val cpgVersion       = "1.3.314"
val scalatestVersion = "3.1.1"

Test / fork := true
ThisBuild / resolvers ++= Seq(
  Resolver.mavenLocal,
  "Atlassian Maven Repository" at "https://maven.atlassian.com/repository/public",
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"
)

ThisBuild / trapExit := false

ThisBuild / libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"        % cpgVersion,
  "io.shiftleft"  %% "semanticcpg"              % cpgVersion,
  "io.shiftleft"  %% "dataflowengineoss"        % cpgVersion,
  "io.shiftleft"  %% "semanticcpg-tests"        % cpgVersion       % Test classifier "tests",
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.22.1",
  "org.scalatest" %% "scalatest"                % scalatestVersion % Test
)

sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/joernio/javasrc2cpg"),
                                        "scm:git@github.com:joernio/javasrc2cpg.git"))
ThisBuild / homepage := Some(url("https://github.com/joernio/javasrc2cpg/"))
ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer(
    "fabsx00",
    "Fabian Yamaguchi",
    "fabs@shiftleft.io",
    url("https://github.com/fabsx00")
  )
)
ThisBuild / publishMavenStyle := true

enablePlugins(JavaAppPackaging, GitVersioning, BuildInfoPlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
       	 
