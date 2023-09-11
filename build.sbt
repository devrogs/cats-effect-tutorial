name := "cats-effect-tutorial"

version := "3.5.1"

scalaVersion := "2.13.11"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.1" withSources() withJavadoc()

scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-unchecked",
  "-language:postfixOps"
)
