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
        //p( modelpkg.symbol.simpleName ) //this package name = "model"
        //p( modelpkg.symbol.owner.fullName ) // parent package name

        modelpkg.children.foreach( c => c match{
            case clazz @ ClassDef(mods,name,tparam,impl) if hasAnotation(clazz,"org.slim3.datastore.Model") =>
              val meta = new MetaGen(modelpkg.symbol.owner.fullName,name.toString)
              p("<package>"+meta.packageName)
              p(" <model>"+meta.modelName)
              impl.children.foreach( m => m match{
                  case prop @ ValDef(mods,name,tpt,rhs) if hasAnotation(prop,"scala.reflect.BeanProperty") =>
                    p( "  <property>"+prop.name + ":" + prop.symbol.tpe.toLongString )
                    meta.props.put( prop.name.toString.trim, prop.symbol.tpe.toString )
                    meta.annotations.put(prop.name.toString.trim, prop.symbol.annotations.map(_.toString))
                    p("  <annotations>" + prop.symbol.annotations.map(_.toString()).mkString(","))
                  case method @ DefDef(mods,name,tparams,vparams,tpt,rhs) if isJavaAccessor(method) =>
                    ()//p( "  <accessor>"+method.name )
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