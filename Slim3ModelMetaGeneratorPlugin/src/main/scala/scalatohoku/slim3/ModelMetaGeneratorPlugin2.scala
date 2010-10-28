package scalatohoku.slim3

import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

/**
 * 実験中。
 * ASTを書き換えてMetaクラスを生成する。
 */
class ModelMetaGeneratorPlugin2(val global: Global) extends Plugin {

  val name = "slim3-meta-generate"
  val description = "genarate slim3 ModelMeta on compiling models"

  val components = List[PluginComponent](new ModelMetaTransformComponent(global))
  
}
