name := "javasrc2cpg"
organization := "io.joern"

scalaVersion := "2.13.6"

val cpgVersion       = "1.3.380"
val scalatestVersion = "3.1.1"

Test / fork := true
resolvers ++= Seq(
  Resolver.mavenLocal,
  "Atlassian Maven Repository" at "https://maven.atlassian.com/repository/public",
  "Sonatype OSS" at "https://oss.sonatype.org/content/repositories/public"
)

trapExit := false

scalacOptions ++= Seq(
  "-deprecation" // Emit warning and location for usages of deprecated APIs.
)

libraryDependencies ++= Seq(
  "io.shiftleft"  %% "codepropertygraph"        % cpgVersion,
  "io.shiftleft"  %% "semanticcpg"              % cpgVersion,
  "io.shiftleft"  %% "dataflowengineoss"        % cpgVersion,
  "io.shiftleft"  %% "semanticcpg-tests"        % cpgVersion       % Test classifier "tests",
  "com.github.javaparser" % "javaparser-symbol-solver-core" % "3.22.1",
  "org.scalatest" %% "scalatest"                % scalatestVersion % Test
)

sonatypeCredentialHost := "s01.oss.sonatype.org"
publishTo := sonatypePublishToBundle.value
scmInfo := Some(ScmInfo(url("https://github.com/joernio/javasrc2cpg"),
                            "scm:git@github.com:joernio/javasrc2cpg.git"))
homepage := Some(url("https://github.com/joernio/javasrc2cpg/"))
licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
developers := List(
  Developer(
    "fabsx00",
    "Fabian Yamaguchi",
    "fabs@shiftleft.io",
    url("https://github.com/fabsx00")
  )
)

enablePlugins(JavaAppPackaging, GitVersioning, BuildInfoPlugin)

Global / onChangedBuildSource := ReloadOnSourceChanges
       	 
