import sbt._

class Slim3ModelTestProject(info: ProjectInfo) extends DefaultProject(info)
{
  override def compileOptions = super.compileOptions ++
  compileOptions("-Xplugin:../Slim3ModelMetaGeneratorPlugin/target/scala_2.8.0/slim3modelmetageneratorplugin_2.8.0-0.1.jar")
  //compileOptions("-Yshow-trees" ,"-Xprint:refchecks")
}
