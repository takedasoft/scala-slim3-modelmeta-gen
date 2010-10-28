package scalatohoku.slim3

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.ast.TreeBrowsers
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class ModelMetaGeneratorPlugin(val global: Global) extends Plugin {
  import global._

  val name = "slim3-meta-generate"
  val description = "genarate slim3 ModelMeta on compiling models"
  val components = List[PluginComponent](Component)
  
  private object Component extends PluginComponent {
    val global: ModelMetaGeneratorPlugin.this.global.type = ModelMetaGeneratorPlugin.this.global
    val runsAfter = List[String]("refchecks");
    val phaseName = ModelMetaGeneratorPlugin.this.name
    def newPhase(_prev: Phase) = new GeneratePhase(_prev)    
    
    class GeneratePhase(prev: Phase) extends StdPhase(prev) {
      override def name = ModelMetaGeneratorPlugin.this.name

      def apply(unit: CompilationUnit) {
        unit.body.find( (p) => p.isInstanceOf[PackageDef] ) match {
          case Some(modelpkg) if isModelPackage(modelpkg) =>
            gen(modelpkg.asInstanceOf[PackageDef])
          case _ => ()
        }
      }

      private def gen( modelpkg:PackageDef ):Unit = {
        p( modelpkg.symbol.simpleName ) //this package name = "model"
        p( modelpkg.symbol.owner.fullName ) // parent package name

        val meta = new Meta
        meta.packageName = modelpkg.symbol.owner.fullName

        modelpkg.children.foreach( c => c match{
            case clazz @ ClassDef(mods,name,tparam,impl) if hasAnotation(clazz,"org.slim3.datastore.Model") =>
              p(name) // model name
              meta.modelName = name.toString
              impl.children.foreach( m => m match{
                  case prop @ ValDef(mods,name,tpt,rhs) if hasAnotation(prop,"scala.reflect.BeanProperty") =>
                    p( prop.name + ":" + prop.symbol.tpe.toLongString ) // model properties
                    meta.props.put( prop.name.toString.trim, prop.symbol.tpe.toString )
                  case method @ DefDef(mods,name,tparams,vparams,tpt,rhs) if isJavaAccessor(method) =>
                    p( method.name ) // accessors
                  case _ => ()
               } )
              meta.generateSourceFile
            case _ => ()
          } )
      }

      private def isJavaAccessor( dd:DefDef ) = dd.name.startsWith("get") || dd.name.startsWith("set") || dd.name.startsWith("add")
      private def isModelPackage( pd:Tree ) = pd.symbol.simpleName.toString.equals("model")
      private def hasAnotation(t:Tree,clazz:String):Boolean = t.symbol.annotations.count(a=>a.toString.startsWith(clazz)) > 0
      
      private def p(o:Any):Unit = System.out.println("[slim3-gen]"+o)
      private def b(t:Tree):Unit = ModelMetaGeneratorPlugin.this.global.treeBrowsers.create().browse(t)
    }
  }
}

class Meta {

  val props = new scala.collection.mutable.HashMap[String,String] // name -> className
  var packageName:String = _
  var modelName:String = _

  def generateSourceFile() {
    import java.io._
    val metaSrcDir = "src/main/scala/"+packageName+"/meta"
    val dir = new File(metaSrcDir)
    if( ! dir.exists ) dir.mkdir
    
    val file = new FileWriter( metaSrcDir+"/"+modelName+"Meta.scala")
    file.write(contents)
    file.close
  }

  private def contents:String = {
    val valsSrc = props.keys.map( k => props(k) match {
      case "String" =>
        Template.PropString.replace("$$propname$$",k)
      case "com.google.appengine.api.datastore.Key" =>
        Template.PropKey
      case clazz =>
        Template.PropCore.replace("$$propname$$",k).replace("$$typename$$", clazz)
    } )
    val modelToEntitySrc  = props.keys.map( k =>
      Template.ModelToEntity.replace("$$propname$$",k).replace("$$getter$$",toGetter(k))
    )
    val entityToModelSrc  = props.keys.map( k =>
      Template.EntityToModel.replace("$$propname$$",k).replace("$$setter$$",toSetter(k)).replace("$$typename$$", props(k))
    )
    Template.Meta
    .replace("$$instance_variables$$",valsSrc.mkString)
    .replace("$$entity_to_model$$",entityToModelSrc.mkString)
    .replace("$$model_to_entity$$",modelToEntitySrc.mkString)
    .replace("$$modelname$$",modelName)
    .replace("$$package$$",packageName)
  }

  private def toGetter(s:String) = "get" + toCamelCase(s)
  private def toSetter(s:String) = "set" + toCamelCase(s)
  private def toCamelCase(s:String) = s.substring(0, 1).toUpperCase() + s.substring(1)

  object Template {

    val PropString = """
  val $$propname$$ = new StringAttributeMeta[$$modelname$$](this, "$$propname$$", "$$propname$$");"""
    val PropCore = """
  val $$propname$$ = new CoreAttributeMeta[$$modelname$$,$$typename$$](this, "$$propname$$", "$$propname$$",classOf[$$typename$$]);"""
    val PropKey = """
  val key = new CoreAttributeMeta[$$modelname$$,Key](this, "__key__", "key", classOf[Key]);"""

    val EntityToModel = """
    model.$$setter$$(entity.getProperty("$$propname$$").asInstanceOf[$$typename$$]);"""
    val ModelToEntity = """
    entity.setProperty("$$propname$$", m.$$getter$$());"""
  
    val Meta = """// auto generated by ModelMetaGeneratorPlugin
package $$package$$.meta

import $$package$$.model.$$modelname$$
import org.slim3.datastore._
import com.google.appengine.api.datastore._

class $$modelname$$Meta extends ModelMeta[$$modelname$$]("$$modelname$$", classOf[$$modelname$$]) {

$$instance_variables$$
  
  override def entityToModel(entity:Entity) = {
    val model = new $$modelname$$
    model.setKey(entity.getKey());
    model.setVersion(entity.getProperty("version").asInstanceOf[Long])
$$entity_to_model$$
    model
  }

  override def modelToEntity(model:Any) = {
    val m = model.asInstanceOf[$$modelname$$]
    val entity = if ( m.getKey() != null ) { new Entity(m.getKey())} else {new Entity(kind)}
    entity.setProperty("version", m.getVersion());
    entity.setProperty("slim3.schemaVersion", 1);
$$model_to_entity$$
    entity
  }

  override def getKey(model:Any) = {
    model.asInstanceOf[$$modelname$$].getKey();
  }
  override def setKey(model:Any, key:Key):Unit = {
    validateKey(key);
    model.asInstanceOf[$$modelname$$].setKey(key);
  }
  override def getVersion(model:Any):Long = {
    model.asInstanceOf[$$modelname$$].getVersion
  }
  override def incrementVersion(model:Any):Unit = {
    val m = model.asInstanceOf[$$modelname$$]
    val version = m.getVersion()
    m.setVersion( version + 1 )
  }
  override def prePut(model:Any):Unit = {
    assignKeyIfNecessary(model)
    incrementVersion(model)
  }

  override def getSchemaVersionName = "slim3.schemaVersion"
  override def getClassHierarchyListName = "slim3.classHierarchyList";
}
object $$modelname$$Meta extends $$modelname$$Meta{
  def get = this
}
"""
  } //end of Template
}