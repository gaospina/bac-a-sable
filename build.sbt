name := "Bac À Sable II"

version := "0.1"

scalaVersion := "2.13.2"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xdisable-assertions",
  "-language:implicitConversions",
  "-language:postfixOps"
)

libraryDependencies ++= Seq(
  "org.jdesktop.swingx" % "jxmapviewer2" % "1.3.1",
  "org.jfree" % "jfreechart" % "1.5.0",
  "org.locationtech.jts" % "jts-core" % "1.16.1",
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0"
)

// Cross-version for parallel collections
libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major <= 12 =>
      Seq()
    case _ =>
      Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
  }
}