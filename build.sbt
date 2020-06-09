val id = "123x456x" // 学籍番号を入れて下さい

name := s"prolang-sp-$id"

version := "1.2.8"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"

unmanagedResourceDirectories in Compile += baseDirectory.value / "docs"
unmanagedResourceDirectories in Compile += baseDirectory.value / "CspFiles"

lazy val root = (project in file(".")).
  settings(
    mainClass in assembly := Some("cp1.plspSolver"),
    assemblyJarName in assembly := s"$id.jar",
    name := "plspSolver"
  )

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case _ => MergeStrategy.first
}