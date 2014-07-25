organization := "scalatohoku"

name := "Slim3ModelTest"

version := "0.1"

scalaVersion := "2.9.1"

autoCompilerPlugins := true

addCompilerPlugin("scalatohoku.slim3" %% "slim3modelmetageneratorplugin" % "0.1")

libraryDependencies ++= Seq(
  "org.slim3" % "slim3" % "1.0.13"
)

resolvers ++= Seq(
  "seasar" at "https://www.seasar.org/maven/maven2"
)

