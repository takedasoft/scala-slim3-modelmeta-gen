package scalatohoku.slim3

import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.ast.TreeBrowsers

/**
 * 実験中。
 * ASTを書き換えてMetaクラスを生成する。
 */
class ModelMetaTransformComponent(val global: Global) extends PluginComponent with Transform {

  import global._
  import global.definitions._
  import reflect.generic.Flags._
    
  val runsAfter = List[String]("refchecks");
  val phaseName = "model-meta-transform"
    
  def newTransformer(unit: CompilationUnit) = new GenerateTransformer
    
  class GenerateTransformer extends Transformer {
    override def transform(tree: Tree): Tree = super.transform(preTransform(tree))
      
    def preTransform(tree: Tree): Tree = tree match {
      case modelpkg : PackageDef if isModelPackage(modelpkg) =>
        gen(modelpkg.asInstanceOf[PackageDef])
      case _ =>
        tree
    }

    private def gen( modelpkg:PackageDef ):PackageDef = {
      p( modelpkg.symbol.simpleName ) //このパッケージ
      p( modelpkg.symbol.owner.fullName ) //親パッケージ名
      modelpkg.children.foreach( c => c match{
          case clazz @ ClassDef(mods,name,tparam,impl) if hasAnotation(clazz,"org.slim3.datastore.Model") =>
            p(name) //モデルクラス名
            impl.children.foreach( m => m match{
                case prop @ ValDef(mods,name,tpt,rhs) if hasAnotation(prop,"scala.reflect.BeanProperty") =>
                  p( prop ) //インスタンス変数
                case method @ DefDef(mods,name,tparams,vparams,tpt,rhs) if isJavaAccessor(method) =>
                  p( method ) //メソッド
                case _ => ()
              } )
          case _ => ()
        } )
      modelpkg
    }

    private def isJavaAccessor( dd:DefDef ) = dd.name.startsWith("get") || dd.name.startsWith("set") || dd.name.startsWith("add")
    private def isModelPackage( pd:Tree ) = pd.symbol.simpleName.toString.equals("model")
    private def hasAnotation(t:Tree,clazz:String):Boolean = t.symbol.annotations.count(a=>a.toString.startsWith(clazz)) > 0
      
    private def p(o:Any):Unit = System.out.println(o)
    //private def b(t:Tree):Unit = ModelMetaGenerator.this.global.treeBrowsers.create().browse(t)

/*
    def tmpl:Tree={
      PackageDef(
        Ident("<empty>") // sym=package <empty>, sym.owner=package <root>, sym.tpe=package <empty>, tpe=type, tpe.sym=package <empty>, tpe.sym.owner=package <root>
        ,
        //ここにmodelpkgそのもの
        // ,
        //
        List(metaTmpl)
      )
    }

    def metaTmpl:Tree = {
      PackageDef(
        Select( // sym=package meta, sym.owner=package tutorial, sym.tpe=package tutorial.meta, tpe=tutorial.meta.type, tpe.sym=package meta, tpe.sym.owner=package tutorial
          Ident("tutorial"), // sym=package tutorial, sym.owner=package <root>, sym.tpe=package tutorial, tpe=tutorial.type, tpe.sym=package tutorial, tpe.sym.owner=package <root>,
          "meta")
        ,
        List( metaClassTmpl,metaObjectTmpl)
       )
    }
    def metaClassTmpl:Tree = {
        ClassDef( // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=<notype>, tpe.sym=<none>
          Modifiers( MODULEVAR | SYNTHETICMETH | MONOMORPHIC ), // flags=, annots=List()
          "TweetMeta",
          List(), // no type parameter
          Template( // sym=value <local TweetMeta>, sym.owner=class TweetMeta, sym.tpe=<notype>, tpe=tutorial.meta.TweetMeta, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
            //List(class ModelMeta, trait ScalaObject), // parents
            List(),
            ValDef( // sym=<none>, sym.tpe=<notype>, tpe=<notype>, tpe.sym=<none>
              Modifiers( 0 ), // flags=, annots=List()
              "_",
              TypeTree(), // sym=<none>, tpe=<notype>, tpe.sym=<none>,
              EmptyTree
            ),
            List( // body
              DefDef( // sym=constructor TweetMeta, isPrimaryConstructor, sym.owner=class TweetMeta, sym.tpe=()tutorial.meta.TweetMeta, tpe=<notype>, tpe.sym=<none>
                Modifiers( METHOD ), // flags=<method>, annots=List()
                "<init>",
                List(), // no type parameter
                List(List()), // no parameter
                metaClassTmpl, //"tutorial.meta.TweetMeta",
                Block( // sym=null, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                  List( // 1 statement(s)
                    Apply( // sym=constructor ModelMeta, tpe=org.slim3.datastore.ModelMeta[tutorial.model.Tweet], tpe.sym=class ModelMeta, tpe.sym.owner=package datastore
                      Select( // sym=constructor ModelMeta, isPrimaryConstructor, sym.owner=class ModelMeta, sym.tpe=(x$1: java.lang.String,x$2: java.lang.Class[M])org.slim3.datastore.ModelMeta[M], tpe=(x$1: java.lang.String,x$2: java.lang.Class[tutorial.model.Tweet])org.slim3.datastore.ModelMeta[tutorial.model.Tweet], tpe.sym=<none>
                        Super("", ""), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.super.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(variable kind: java.lang.String, , variable modelClass: java.lang.Class[M], , variable classHierarchyList: java.util.List[java.lang.String], , variable beanDesc: org.slim3.util.BeanDesc, , constructor ModelMeta: (x$1: java.lang.String,x$2: java.lang.Class[M])org.slim3.datastore.ModelMeta[M], , constructor ModelMeta: (x$1: java.lang.String,x$2: java.lang.Class[M],x$3: java.util.List[java.lang.String])org.slim3.datastore.ModelMeta[M], , constructor ModelMeta: ()org.slim3.datastore.ModelMeta[M], , method getKind: ()java.lang.String, , method getModelClass: ()java.lang.Class[M], , method getClassHierarchyList: ()java.util.List[java.lang.String], , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, , method entityToModel: (x$1: com.google.appengine.api.datastore.Entity)M, , method modelToEntity: (x$1: Any)com.google.appengine.api.datastore.Entity, , method getVersion: (x$1: Any)Long, , method incrementVersion: (x$1: Any)Unit, , method prePut: (x$1: Any)Unit, , method getKey: (x$1: Any)com.google.appengine.api.datastore.Key, , method setKey: (x$1: Any,x$2: com.google.appengine.api.datastore.Key)Unit, , method validateKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method assignKeyIfNecessary: (x$1: Any)com.google.appengine.api.datastore.Key, , method longToPrimitiveShort: (x$1: java.lang.Long)Short, , method longToShort: (x$1: java.lang.Long)java.lang.Short, , method longToPrimitiveInt: (x$1: java.lang.Long)Int, , method longToInteger: (x$1: java.lang.Long)java.lang.Integer, , method longToPrimitiveLong: (x$1: java.lang.Long)Long, , method doubleToPrimitiveFloat: (x$1: java.lang.Double)Float, , method doubleToFloat: (x$1: java.lang.Double)java.lang.Float, , method doubleToPrimitiveDouble: (x$1: java.lang.Double)Double, , method booleanToPrimitiveBoolean: (x$1: java.lang.Boolean)Boolean, , method enumToString: (x$1: java.lang.Enum[_])java.lang.String, , method stringToEnum: [T <: java.lang.Enum[T]](x$1: java.lang.Class[T],x$2: java.lang.String)T, , method textToString: (x$1: com.google.appengine.api.datastore.Text)java.lang.String, , method stringToText: (x$1: java.lang.String)com.google.appengine.api.datastore.Text, , method shortBlobToBytes: (x$1: com.google.appengine.api.datastore.ShortBlob)Array[Byte], , method bytesToShortBlob: (x$1: Array[Byte])com.google.appengine.api.datastore.ShortBlob, , method blobToBytes: (x$1: com.google.appengine.api.datastore.Blob)Array[Byte], , method bytesToBlob: (x$1: Array[Byte])com.google.appengine.api.datastore.Blob, , method shortBlobToSerializable: [T](x$1: com.google.appengine.api.datastore.ShortBlob)T, , method serializableToShortBlob: (x$1: Any)com.google.appengine.api.datastore.ShortBlob, , method blobToSerializable: [T](x$1: com.google.appengine.api.datastore.Blob)T, , method serializableToBlob: (x$1: Any)com.google.appengine.api.datastore.Blob, , method toList: [T](x$1: java.lang.Class[T],x$2: Any)java.util.ArrayList[T], , method longListToShortList: (x$1: Any)java.util.ArrayList[java.lang.Short], , method longListToIntegerList: (x$1: Any)java.util.ArrayList[java.lang.Integer], , method doubleListToFloatList: (x$1: Any)java.util.ArrayList[java.lang.Float], , method enumListToStringList: (x$1: Any)java.util.List[java.lang.String], , method stringListToEnumList: [T <: java.lang.Enum[T]](x$1: java.lang.Class[T],x$2: Any)java.util.List[T], , method getBeanDesc: ()org.slim3.util.BeanDesc, ),
                        "<init>"),
                      List( // 2 arguments(s)
                        Literal(Constant("Tweet")),
                        Literal(Constant("tutorial.model.Tweet"))
                      )
                    )
                  ),
                  Literal(Constant(()))
                )
              ),
              ValDef( // sym=value content, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe=<notype>, tpe.sym=<none>
                Modifiers(PRIVATE | LOCAL), // flags=private <local>, annots=List()
                "content ",
                TypeTree(), // sym=class StringAttributeMeta, tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe.sym=class StringAttributeMeta, tpe.sym.owner=package datastore,
                Apply( // sym=constructor StringAttributeMeta, tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe.sym=class StringAttributeMeta, tpe.sym.owner=package datastore
                  Select( // sym=constructor StringAttributeMeta, isPrimaryConstructor, sym.owner=class StringAttributeMeta, sym.tpe=(x$1: org.slim3.datastore.ModelMeta[M],x$2: java.lang.String,x$3: java.lang.String)org.slim3.datastore.StringAttributeMeta[M], tpe=(x$1: org.slim3.datastore.ModelMeta[tutorial.model.Tweet],x$2: java.lang.String,x$3: java.lang.String)org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe.sym=<none>
                    New( // sym=null, tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe.sym=class StringAttributeMeta, tpe.sym.owner=package datastore
                      TypeTree() // sym=class StringAttributeMeta, tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe.sym=class StringAttributeMeta, tpe.sym.owner=package datastore
                    ),
                    "<init>"),
                  List( // 3 arguments(s)
                    This(""), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=tutorial.meta.TweetMeta, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                    Literal(Constant("content")),
                    Literal(Constant("content"))
                  )
                )
              ),
              DefDef( // sym=value content, sym.owner=class TweetMeta, sym.tpe==> org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe=<notype>, tpe.sym=<none>
                Modifiers(METHOD | STABLE | ACCESSOR), // flags=<method> <stable> <accessor>, annots=List()
                "content",
                List(), // no type parameter
                List(
                ),
                "org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet]",
                Select( // sym=value content, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe=org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], tpe.sym=class StringAttributeMeta, tpe.sym.owner=package datastore
                  This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                  "content ")
              ),
              ValDef( // sym=value createdDate, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe=<notype>, tpe.sym=<none>
                PRIVATE | LOCAL, // flags=private <local>, annots=List()
                "createdDate ",
                TypeTree(), // sym=class CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore,
                Apply( // sym=constructor CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                  Select( // sym=constructor CoreAttributeMeta, isPrimaryConstructor, sym.owner=class CoreAttributeMeta, sym.tpe=(x$1: org.slim3.datastore.ModelMeta[M],x$2: java.lang.String,x$3: java.lang.String,x$4: java.lang.Class[A])org.slim3.datastore.CoreAttributeMeta[M,A], tpe=(x$1: org.slim3.datastore.ModelMeta[tutorial.model.Tweet],x$2: java.lang.String,x$3: java.lang.String,x$4: java.lang.Class[java.util.Date])org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe.sym=<none>
                    New( // sym=null, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                      TypeTree() // sym=class CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                    ),
                    "<init>"),
                  List( // 4 arguments(s)
                    This(""), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=tutorial.meta.TweetMeta, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                    Literal(Constant(createdDate)),
                    Literal(Constant(createdDate)),
                    Literal(Constant(java.util.Date))
                  )
                )
              ),
              DefDef( // sym=value createdDate, sym.owner=class TweetMeta, sym.tpe==> org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe=<notype>, tpe.sym=<none>
                METHOD | STABLE | ACCESSOR, // flags=<method> <stable> <accessor>, annots=List()
                "createdDate",
                List(), // no type parameter
                List(
                ),
                org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date],
                Select( // sym=value createdDate, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                  This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                  "createdDate ")
              ),
              ValDef( // sym=value key, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe=<notype>, tpe.sym=<none>
                PRIVATE | LOCAL, // flags=private <local>, annots=List()
                "key ",
                TypeTree(), // sym=class CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore,
                Apply( // sym=constructor CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                  Select( // sym=constructor CoreAttributeMeta, isPrimaryConstructor, sym.owner=class CoreAttributeMeta, sym.tpe=(x$1: org.slim3.datastore.ModelMeta[M],x$2: java.lang.String,x$3: java.lang.String,x$4: java.lang.Class[A])org.slim3.datastore.CoreAttributeMeta[M,A], tpe=(x$1: org.slim3.datastore.ModelMeta[tutorial.model.Tweet],x$2: java.lang.String,x$3: java.lang.String,x$4: java.lang.Class[com.google.appengine.api.datastore.Key])org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe.sym=<none>
                    New( // sym=null, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                      TypeTree() // sym=class CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                    ),
                    "<init>"),
                  List( // 4 arguments(s)
                    This(""), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=tutorial.meta.TweetMeta, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                    Literal(Constant(__key__)),
                    Literal(Constant(key)),
                    Literal(Constant(com.google.appengine.api.datastore.Key))
                  )
                )
              ),
              DefDef( // sym=value key, sym.owner=class TweetMeta, sym.tpe==> org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe=<notype>, tpe.sym=<none>
                METHOD | STABLE | ACCESSOR, // flags=<method> <stable> <accessor>, annots=List()
                "key",
                List(), // no type parameter
                List(
                ),
                org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key],
                Select( // sym=value key, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                  This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                  "key ")
              ),
              ValDef( // sym=value version, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe=<notype>, tpe.sym=<none>
                PRIVATE | LOCAL, // flags=private <local>, annots=List()
                "version ",
                TypeTree(), // sym=class CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore,
                Apply( // sym=constructor CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                  Select( // sym=constructor CoreAttributeMeta, isPrimaryConstructor, sym.owner=class CoreAttributeMeta, sym.tpe=(x$1: org.slim3.datastore.ModelMeta[M],x$2: java.lang.String,x$3: java.lang.String,x$4: java.lang.Class[A])org.slim3.datastore.CoreAttributeMeta[M,A], tpe=(x$1: org.slim3.datastore.ModelMeta[tutorial.model.Tweet],x$2: java.lang.String,x$3: java.lang.String,x$4: java.lang.Class[Long])org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe.sym=<none>
                    New( // sym=null, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                      TypeTree() // sym=class CoreAttributeMeta, tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                    ),
                    "<init>"),
                  List( // 4 arguments(s)
                    This(""), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=tutorial.meta.TweetMeta, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                    Literal(Constant(version)),
                    Literal(Constant(version)),
                    Literal(Constant(Long))
                  )
                )
              ),
              DefDef( // sym=value version, sym.owner=class TweetMeta, sym.tpe==> org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe=<notype>, tpe.sym=<none>
                METHOD | STABLE | ACCESSOR, // flags=<method> <stable> <accessor>, annots=List()
                "version",
                List(), // no type parameter
                List(
                ),
                org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long],
                Select( // sym=value version, sym.owner=class TweetMeta, sym.tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe=org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], tpe.sym=class CoreAttributeMeta, tpe.sym.owner=package datastore
                  This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                  "version ")
              ),
              DefDef( // sym=method entityToModel, sym.owner=class TweetMeta, sym.tpe=(entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "entityToModel",
                List(), // no type parameter
                List(
                  List( // 1 parameter(s)
                    ValDef( // sym=value entity, sym.owner=method entityToModel, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "entity",
                      TypeTree(), // sym=class Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore,
                      EmptyTree
                    )
                  )
                ),
                tutorial.model.Tweet,
                Block( // sym=null, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                  List( // 5 statement(s)
                    ValDef( // sym=value model, sym.owner=method entityToModel, sym.tpe=tutorial.model.Tweet, tpe=<notype>, tpe.sym=<none>
                      0, // flags=, annots=List()
                      "model",
                      TypeTree(), // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                      Apply( // sym=constructor Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        Select( // sym=constructor Tweet, isPrimaryConstructor, sym.owner=class Tweet, sym.tpe=()tutorial.model.Tweet, tpe=()tutorial.model.Tweet, tpe.sym=<none>
                          New( // sym=null, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                            Select( // sym=class Tweet, sym.owner=package model, sym.tpe=tutorial.model.Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                              Select( // sym=package model, sym.owner=package tutorial, sym.tpe=package tutorial.model, tpe=tutorial.model.type, tpe.sym=package model, tpe.sym.owner=package tutorial
                                Ident("tutorial"), // sym=package tutorial, sym.owner=package <root>, sym.tpe=package tutorial, tpe=tutorial.type, tpe.sym=package tutorial, tpe.sym.owner=package <root>,
                                "model"),
                              "Tweet")
                          ),
                          "<init>"),
                        Nil // no argument
                      )
                    ),
                    Apply( // sym=method setKey, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setKey, sym.owner=class Tweet, sym.tpe=(x$1: com.google.appengine.api.datastore.Key)Unit, tpe=(x$1: com.google.appengine.api.datastore.Key)Unit, tpe.sym=<none>
                        Ident("model"), // sym=value model, sym.owner=method entityToModel, sym.tpe=tutorial.model.Tweet, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                        "setKey"),
                      List( // 1 arguments(s)
                        Apply( // sym=method getKey, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                          Select( // sym=method getKey, sym.owner=class Entity, sym.tpe=()com.google.appengine.api.datastore.Key, tpe=()com.google.appengine.api.datastore.Key, tpe.sym=<none>
                            Ident("entity"), // sym=value entity, sym.owner=method entityToModel, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                            "getKey"),
                          Nil // no argument
                        )
                      )
                    ),
                    Apply( // sym=method setContent, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setContent, sym.owner=class Tweet, sym.tpe=(x$1: String)Unit, tpe=(x$1: String)Unit, tpe.sym=<none>
                        Ident("model"), // sym=value model, sym.owner=method entityToModel, sym.tpe=tutorial.model.Tweet, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                        "setContent"),
                      List( // 1 arguments(s)
                        TypeApply( // sym=method asInstanceOf, tpe=String, tpe.sym=class String, tpe.sym.owner=package lang
                          Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                            Apply( // sym=method getProperty, tpe=java.lang.Object, tpe.sym=class Object, tpe.sym.owner=package lang
                              Select( // sym=method getProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String)java.lang.Object, tpe=(x$1: java.lang.String)java.lang.Object, tpe.sym=<none>
                                Ident("entity"), // sym=value entity, sym.owner=method entityToModel, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                                "getProperty"),
                              List( // 1 arguments(s)
                                Literal(Constant(content))
                              )
                            ),
                            "asInstanceOf"),
                          List(
                            TypeTree() // sym=class String, tpe=String, tpe.sym=class String, tpe.sym.owner=package lang
                          )
                        )
                      )
                    ),
                    Apply( // sym=method setCreatedDate, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setCreatedDate, sym.owner=class Tweet, sym.tpe=(x$1: java.util.Date)Unit, tpe=(x$1: java.util.Date)Unit, tpe.sym=<none>
                        Ident("model"), // sym=value model, sym.owner=method entityToModel, sym.tpe=tutorial.model.Tweet, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                        "setCreatedDate"),
                      List( // 1 arguments(s)
                        TypeApply( // sym=method asInstanceOf, tpe=java.util.Date, tpe.sym=class Date, tpe.sym.owner=package util
                          Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                            Apply( // sym=method getProperty, tpe=java.lang.Object, tpe.sym=class Object, tpe.sym.owner=package lang
                              Select( // sym=method getProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String)java.lang.Object, tpe=(x$1: java.lang.String)java.lang.Object, tpe.sym=<none>
                                Ident("entity"), // sym=value entity, sym.owner=method entityToModel, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                                "getProperty"),
                              List( // 1 arguments(s)
                                Literal(Constant(createdDate))
                              )
                            ),
                            "asInstanceOf"),
                          List(
                            TypeTree() // sym=class Date, tpe=java.util.Date, tpe.sym=class Date, tpe.sym.owner=package util
                          )
                        )
                      )
                    ),
                    Apply( // sym=method setVersion, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setVersion, sym.owner=class Tweet, sym.tpe=(x$1: Long)Unit, tpe=(x$1: Long)Unit, tpe.sym=<none>
                        Ident("model"), // sym=value model, sym.owner=method entityToModel, sym.tpe=tutorial.model.Tweet, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                        "setVersion"),
                      List( // 1 arguments(s)
                        TypeApply( // sym=method asInstanceOf, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala
                          Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                            Apply( // sym=method getProperty, tpe=java.lang.Object, tpe.sym=class Object, tpe.sym.owner=package lang
                              Select( // sym=method getProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String)java.lang.Object, tpe=(x$1: java.lang.String)java.lang.Object, tpe.sym=<none>
                                Ident("entity"), // sym=value entity, sym.owner=method entityToModel, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method entityToModel, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                                "getProperty"),
                              List( // 1 arguments(s)
                                Literal(Constant(version))
                              )
                            ),
                            "asInstanceOf"),
                          List(
                            TypeTree() // sym=class Long, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala
                          )
                        )
                      )
                    )
                  ),
                  Ident("model") // sym=value model, sym.owner=method entityToModel, sym.tpe=tutorial.model.Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                )
              ),
              DefDef( // sym=method modelToEntity, sym.owner=class TweetMeta, sym.tpe=(model: Any)com.google.appengine.api.datastore.Entity, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "modelToEntity",
                List(), // no type parameter
                List(
                  List( // 1 parameter(s)
                    ValDef( // sym=value model, sym.owner=method modelToEntity, sym.tpe=Any, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "model",
                      TypeTree(), // sym=class Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala,
                      EmptyTree
                    )
                  )
                ),
                com.google.appengine.api.datastore.Entity,
                Block( // sym=null, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                  List( // 6 statement(s)
                    ValDef( // sym=value m, sym.owner=method modelToEntity, sym.tpe=tutorial.model.Tweet, tpe=<notype>, tpe.sym=<none>
                      0, // flags=, annots=List()
                      "m",
                      TypeTree(), // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                      TypeApply( // sym=method asInstanceOf, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                          Ident("model"), // sym=value model, sym.owner=method modelToEntity, sym.tpe=Any, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method modelToEntity, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, ),
                          "asInstanceOf"),
                        List(
                          TypeTree() // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        )
                      )
                    ),
                    ValDef( // sym=value entity, sym.owner=method modelToEntity, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=<notype>, tpe.sym=<none>
                      0, // flags=, annots=List()
                      "entity",
                      TypeTree(), // sym=class Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore,
                      If(
                        Apply( // sym=method !=, tpe=Boolean, tpe.sym=class Boolean, tpe.sym.owner=package scala
                          Select( // sym=method !=, sym.owner=class Object, sym.tpe=(x$1: AnyRef)Boolean, tpe=(x$1: AnyRef)Boolean, tpe.sym=<none>
                            Apply( // sym=method getKey, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                              Select( // sym=method getKey, sym.owner=class Tweet, sym.tpe=()com.google.appengine.api.datastore.Key, tpe=()com.google.appengine.api.datastore.Key, tpe.sym=<none>
                                Ident("m"), // sym=value m, sym.owner=method modelToEntity, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                                "getKey"),
                              Nil // no argument
                            ),
                            "$bang$eq"),
                          List( // 1 arguments(s)
                            Literal(Constant(null))
                          )
                        )
                        Apply( // sym=constructor Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                          Select( // sym=constructor Entity, isConstructor, sym.owner=class Entity, sym.tpe=(x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, tpe=(x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, tpe.sym=<none>
                            New( // sym=null, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                              Select( // sym=class Entity, sym.owner=package datastore, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                                Select( // sym=package datastore, sym.owner=package api, sym.tpe=package com.google.appengine.api.datastore, tpe=com.google.appengine.api.datastore.type, tpe.sym=package datastore, tpe.sym.owner=package api
                                  Select( // sym=package api, sym.owner=package appengine, sym.tpe=package com.google.appengine.api, tpe=com.google.appengine.api.type, tpe.sym=package api, tpe.sym.owner=package appengine
                                    Select( // sym=package appengine, sym.owner=package google, sym.tpe=package com.google.appengine, tpe=com.google.appengine.type, tpe.sym=package appengine, tpe.sym.owner=package google
                                      Select( // sym=package google, sym.owner=package com, sym.tpe=package com.google, tpe=com.google.type, tpe.sym=package google, tpe.sym.owner=package com
                                        Ident("com"), // sym=package com, sym.owner=package <root>, sym.tpe=package com, tpe=com.type, tpe.sym=package com, tpe.sym.owner=package <root>,
                                        "google"),
                                      "appengine"),
                                    "api"),
                                  "datastore"),
                                "Entity")
                            ),
                            "<init>"),
                          List( // 1 arguments(s)
                            Apply( // sym=method getKey, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                              Select( // sym=method getKey, sym.owner=class Tweet, sym.tpe=()com.google.appengine.api.datastore.Key, tpe=()com.google.appengine.api.datastore.Key, tpe.sym=<none>
                                Ident("m"), // sym=value m, sym.owner=method modelToEntity, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                                "getKey"),
                              Nil // no argument
                            )
                          )
                        )
                        Apply( // sym=constructor Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                          Select( // sym=constructor Entity, isPrimaryConstructor, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String)com.google.appengine.api.datastore.Entity, tpe=(x$1: java.lang.String)com.google.appengine.api.datastore.Entity, tpe.sym=<none>
                            New( // sym=null, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                              Select( // sym=class Entity, sym.owner=package datastore, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                                Select( // sym=package datastore, sym.owner=package api, sym.tpe=package com.google.appengine.api.datastore, tpe=com.google.appengine.api.datastore.type, tpe.sym=package datastore, tpe.sym.owner=package api
                                  Select( // sym=package api, sym.owner=package appengine, sym.tpe=package com.google.appengine.api, tpe=com.google.appengine.api.type, tpe.sym=package api, tpe.sym.owner=package appengine
                                    Select( // sym=package appengine, sym.owner=package google, sym.tpe=package com.google.appengine, tpe=com.google.appengine.type, tpe.sym=package appengine, tpe.sym.owner=package google
                                      Select( // sym=package google, sym.owner=package com, sym.tpe=package com.google, tpe=com.google.type, tpe.sym=package google, tpe.sym.owner=package com
                                        Ident("com"), // sym=package com, sym.owner=package <root>, sym.tpe=package com, tpe=com.type, tpe.sym=package com, tpe.sym.owner=package <root>,
                                        "google"),
                                      "appengine"),
                                    "api"),
                                  "datastore"),
                                "Entity")
                            ),
                            "<init>"),
                          List( // 1 arguments(s)
                            Select( // sym=variable kind, sym.owner=class ModelMeta, sym.tpe=java.lang.String, tpe=java.lang.String, tpe.sym=class String, tpe.sym.owner=package lang
                              This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                              "kind")
                          )
                        )
                      )
                    ),
                    Apply( // sym=method setProperty, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe.sym=<none>
                        Ident("entity"), // sym=value entity, sym.owner=method modelToEntity, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                        "setProperty"),
                      List( // 2 arguments(s)
                        Literal(Constant(content)),
                        Apply( // sym=method getContent, tpe=String, tpe.sym=class String, tpe.sym.owner=package lang
                          Select( // sym=method getContent, sym.owner=class Tweet, sym.tpe=()String, tpe=()String, tpe.sym=<none>
                            Ident("m"), // sym=value m, sym.owner=method modelToEntity, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                            "getContent"),
                          Nil // no argument
                        )
                      )
                    ),
                    Apply( // sym=method setProperty, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe.sym=<none>
                        Ident("entity"), // sym=value entity, sym.owner=method modelToEntity, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                        "setProperty"),
                      List( // 2 arguments(s)
                        Literal(Constant(createdDate)),
                        Apply( // sym=method getCreatedDate, tpe=java.util.Date, tpe.sym=class Date, tpe.sym.owner=package util
                          Select( // sym=method getCreatedDate, sym.owner=class Tweet, sym.tpe=()java.util.Date, tpe=()java.util.Date, tpe.sym=<none>
                            Ident("m"), // sym=value m, sym.owner=method modelToEntity, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                            "getCreatedDate"),
                          Nil // no argument
                        )
                      )
                    ),
                    Apply( // sym=method setProperty, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe.sym=<none>
                        Ident("entity"), // sym=value entity, sym.owner=method modelToEntity, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                        "setProperty"),
                      List( // 2 arguments(s)
                        Literal(Constant(version)),
                        Apply( // sym=method getVersion, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala
                          Select( // sym=method getVersion, sym.owner=class Tweet, sym.tpe=()Long, tpe=()Long, tpe.sym=<none>
                            Ident("m"), // sym=value m, sym.owner=method modelToEntity, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                            "getVersion"),
                          Nil // no argument
                        )
                      )
                    ),
                    Apply( // sym=method setProperty, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method setProperty, sym.owner=class Entity, sym.tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe=(x$1: java.lang.String,x$2: Any)Unit, tpe.sym=<none>
                        Ident("entity"), // sym=value entity, sym.owner=method modelToEntity, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=entity.type, tpe.sym=value entity, tpe.sym.owner=method modelToEntity, tpe.decls=List(constructor Entity: (x$1: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: java.lang.String,x$2: java.lang.String,x$3: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , constructor Entity: (x$1: com.google.appengine.api.datastore.Key)com.google.appengine.api.datastore.Entity, , method equals: (x$1: Any)Boolean, , method getKey: ()com.google.appengine.api.datastore.Key, , method getKind: ()java.lang.String, , method getParent: ()com.google.appengine.api.datastore.Key, , method getProperty: (x$1: java.lang.String)java.lang.Object, , method getProperties: ()java.util.Map[java.lang.String,java.lang.Object], , method hashCode: ()Int, , method hasProperty: (x$1: java.lang.String)Boolean, , method removeProperty: (x$1: java.lang.String)Unit, , method setProperty: (x$1: java.lang.String,x$2: Any)Unit, , method setUnindexedProperty: (x$1: java.lang.String,x$2: Any)Unit, , method isUnindexedProperty: (x$1: java.lang.String)Boolean, , method toString: ()java.lang.String, , method getAppId: ()java.lang.String, , method getAppIdNamespace: ()com.google.appengine.api.datastore.AppIdNamespace, , method getNamespace: ()java.lang.String, , method clone: ()com.google.appengine.api.datastore.Entity, , method setPropertiesFrom: (x$1: com.google.appengine.api.datastore.Entity)Unit, , method getPropertyMap: ()java.util.Map[java.lang.String,java.lang.Object], , method setEntityProto: (x$1: com.google.storage.onestore.v3.OnestoreEntity.EntityProto)Unit, , method getEntityProto: ()com.google.storage.onestore.v3.OnestoreEntity.EntityProto, , method clone: ()java.lang.Object, ),
                        "setProperty"),
                      List( // 2 arguments(s)
                        Literal(Constant(slim3.schemaVersion)),
                        Literal(Constant(1))
                      )
                    )
                  ),
                  Ident("entity") // sym=value entity, sym.owner=method modelToEntity, sym.tpe=com.google.appengine.api.datastore.Entity, tpe=com.google.appengine.api.datastore.Entity, tpe.sym=class Entity, tpe.sym.owner=package datastore
                )
              ),
              DefDef( // sym=method getKey, sym.owner=class TweetMeta, sym.tpe=(model: Any)com.google.appengine.api.datastore.Key, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "getKey",
                List(), // no type parameter
                List(
                  List( // 1 parameter(s)
                    ValDef( // sym=value model, sym.owner=method getKey, sym.tpe=Any, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "model",
                      TypeTree(), // sym=class Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala,
                      EmptyTree
                    )
                  )
                ),
                com.google.appengine.api.datastore.Key,
                Apply( // sym=method getKey, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                  Select( // sym=method getKey, sym.owner=class Tweet, sym.tpe=()com.google.appengine.api.datastore.Key, tpe=()com.google.appengine.api.datastore.Key, tpe.sym=<none>
                    TypeApply( // sym=method asInstanceOf, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                      Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                        Ident("model"), // sym=value model, sym.owner=method getKey, sym.tpe=Any, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method getKey, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, ),
                        "asInstanceOf"),
                      List(
                        TypeTree() // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                      )
                    ),
                    "getKey"),
                  Nil // no argument
                )
              ),
              DefDef( // sym=method setKey, sym.owner=class TweetMeta, sym.tpe=(model: Any,key: com.google.appengine.api.datastore.Key)Unit, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "setKey",
                List(), // no type parameter
                List(
                  List( // 2 parameter(s)
                    ValDef( // sym=value model, sym.owner=method setKey, sym.tpe=Any, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "model",
                      TypeTree(), // sym=class Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala,
                      EmptyTree
                    ),
                    ValDef( // sym=value key, sym.owner=method setKey, sym.tpe=com.google.appengine.api.datastore.Key, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "key",
                      TypeTree(), // sym=class Key, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore,
                      EmptyTree
                    )
                  )
                ),
                Unit,
                Block( // sym=null, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                  List( // 1 statement(s)
                    Apply( // sym=method validateKey, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                      Select( // sym=method validateKey, sym.owner=class ModelMeta, sym.tpe=(x$1: com.google.appengine.api.datastore.Key)Unit, tpe=(x$1: com.google.appengine.api.datastore.Key)Unit, tpe.sym=<none>
                        This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                        "validateKey"),
                      List( // 1 arguments(s)
                        Ident("key") // sym=value key, sym.owner=method setKey, sym.tpe=com.google.appengine.api.datastore.Key, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                      )
                    )
                  ),
                  Apply( // sym=method setKey, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                    Select( // sym=method setKey, sym.owner=class Tweet, sym.tpe=(x$1: com.google.appengine.api.datastore.Key)Unit, tpe=(x$1: com.google.appengine.api.datastore.Key)Unit, tpe.sym=<none>
                      TypeApply( // sym=method asInstanceOf, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                          Ident("model"), // sym=value model, sym.owner=method setKey, sym.tpe=Any, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method setKey, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, ),
                          "asInstanceOf"),
                        List(
                          TypeTree() // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        )
                      ),
                      "setKey"),
                    List( // 1 arguments(s)
                      Ident("key") // sym=value key, sym.owner=method setKey, sym.tpe=com.google.appengine.api.datastore.Key, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                    )
                  )
                )
              ),
              DefDef( // sym=method getVersion, sym.owner=class TweetMeta, sym.tpe=(model: Any)Long, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "getVersion",
                List(), // no type parameter
                List(
                  List( // 1 parameter(s)
                    ValDef( // sym=value model, sym.owner=method getVersion, sym.tpe=Any, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "model",
                      TypeTree(), // sym=class Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala,
                      EmptyTree
                    )
                  )
                ),
                Long,
                Apply( // sym=method getVersion, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala
                  Select( // sym=method getVersion, sym.owner=class Tweet, sym.tpe=()Long, tpe=()Long, tpe.sym=<none>
                    TypeApply( // sym=method asInstanceOf, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                      Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                        Ident("model"), // sym=value model, sym.owner=method getVersion, sym.tpe=Any, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method getVersion, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, ),
                        "asInstanceOf"),
                      List(
                        TypeTree() // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                      )
                    ),
                    "getVersion"),
                  Nil // no argument
                )
              ),
              DefDef( // sym=method incrementVersion, sym.owner=class TweetMeta, sym.tpe=(model: Any)Unit, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "incrementVersion",
                List(), // no type parameter
                List(
                  List( // 1 parameter(s)
                    ValDef( // sym=value model, sym.owner=method incrementVersion, sym.tpe=Any, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "model",
                      TypeTree(), // sym=class Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala,
                      EmptyTree
                    )
                  )
                ),
                Unit,
                Block( // sym=null, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                  List( // 2 statement(s)
                    ValDef( // sym=value m, sym.owner=method incrementVersion, sym.tpe=tutorial.model.Tweet, tpe=<notype>, tpe.sym=<none>
                      0, // flags=, annots=List()
                      "m",
                      TypeTree(), // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                      TypeApply( // sym=method asInstanceOf, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        Select( // sym=method asInstanceOf, sym.owner=class Any, sym.tpe=[T0]T0, tpe=[T0]T0, tpe.sym=type T0, tpe.sym.owner=method asInstanceOf, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, )
                          Ident("model"), // sym=value model, sym.owner=method incrementVersion, sym.tpe=Any, tpe=model.type, tpe.sym=value model, tpe.sym.owner=method incrementVersion, tpe.decls=List(method ==: (x$1: Any)Boolean, , method !=: (x$1: Any)Boolean, , method equals: (x$1: Any)Boolean, , method hashCode: ()Int, , method toString: ()java.lang.String, , method ##: ()Int, , method isInstanceOf: [T0]Boolean, , method asInstanceOf: [T0]T0, ),
                          "asInstanceOf"),
                        List(
                          TypeTree() // sym=class Tweet, tpe=tutorial.model.Tweet, tpe.sym=class Tweet, tpe.sym.owner=package model, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, )
                        )
                      )
                    ),
                    ValDef( // sym=value version, sym.owner=method incrementVersion, sym.tpe=Long, tpe=<notype>, tpe.sym=<none>
                      0, // flags=, annots=List()
                      "version",
                      TypeTree(), // sym=class Long, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala,
                      Apply( // sym=method getVersion, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala
                        Select( // sym=method getVersion, sym.owner=class Tweet, sym.tpe=()Long, tpe=()Long, tpe.sym=<none>
                          Ident("m"), // sym=value m, sym.owner=method incrementVersion, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method incrementVersion, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                          "getVersion"),
                        Nil // no argument
                      )
                    )
                  ),
                  Apply( // sym=method setVersion, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                    Select( // sym=method setVersion, sym.owner=class Tweet, sym.tpe=(x$1: Long)Unit, tpe=(x$1: Long)Unit, tpe.sym=<none>
                      Ident("m"), // sym=value m, sym.owner=method incrementVersion, sym.tpe=tutorial.model.Tweet, tpe=m.type, tpe.sym=value m, tpe.sym.owner=method incrementVersion, tpe.decls=List(constructor Tweet: ()tutorial.model.Tweet, , method key: => com.google.appengine.api.datastore.Key, , method key_=: (x$1: com.google.appengine.api.datastore.Key)Unit, , variable key: com.google.appengine.api.datastore.Key, , method getKey: ()com.google.appengine.api.datastore.Key, , method setKey: (x$1: com.google.appengine.api.datastore.Key)Unit, , method version: => Long, , method version_=: (x$1: Long)Unit, , variable version: Long, , method getVersion: ()Long, , method setVersion: (x$1: Long)Unit, , method content: => String, , method content_=: (x$1: String)Unit, , variable content: String, , method getContent: ()String, , method setContent: (x$1: String)Unit, , method createdDate: => java.util.Date, , method createdDate_=: (x$1: java.util.Date)Unit, , variable createdDate: java.util.Date, , method getCreatedDate: ()java.util.Date, , method setCreatedDate: (x$1: java.util.Date)Unit, ),
                      "setVersion"),
                    List( // 1 arguments(s)
                      Apply( // sym=method +, tpe=Long, tpe.sym=class Long, tpe.sym.owner=package scala
                        Select( // sym=method +, sym.owner=class Long, sym.tpe=(x$1: Int)Long, tpe=(x$1: Int)Long, tpe.sym=<none>
                          Ident("version"), // sym=value version, sym.owner=method incrementVersion, sym.tpe=Long, tpe=version.type, tpe.sym=value version, tpe.sym.owner=method incrementVersion, tpe.decls=List(method toByte: => Byte, , method toShort: => Short, , method toChar: => Char, , method toInt: => Int, , method toLong: => Long, , method toFloat: => Float, , method toDouble: => Double, , method +: (x$1: java.lang.String)java.lang.String, , method <<: (x$1: Int)Long, , method <<: (x$1: Long)Long, , method >>>: (x$1: Int)Long, , method >>>: (x$1: Long)Long, , method >>: (x$1: Int)Long, , method >>: (x$1: Long)Long, , method unary_+: => Long, , method unary_-: => Long, , method unary_~: => Long, , method ==: (x$1: Byte)Boolean, , method !=: (x$1: Byte)Boolean, , method <: (x$1: Byte)Boolean, , method <=: (x$1: Byte)Boolean, , method >: (x$1: Byte)Boolean, , method >=: (x$1: Byte)Boolean, , method +: (x$1: Byte)Long, , method -: (x$1: Byte)Long, , method *: (x$1: Byte)Long, , method /: (x$1: Byte)Long, , method %: (x$1: Byte)Long, , method |: (x$1: Byte)Long, , method &: (x$1: Byte)Long, , method ^: (x$1: Byte)Long, , method ==: (x$1: Short)Boolean, , method !=: (x$1: Short)Boolean, , method <: (x$1: Short)Boolean, , method <=: (x$1: Short)Boolean, , method >: (x$1: Short)Boolean, , method >=: (x$1: Short)Boolean, , method +: (x$1: Short)Long, , method -: (x$1: Short)Long, , method *: (x$1: Short)Long, , method /: (x$1: Short)Long, , method %: (x$1: Short)Long, , method |: (x$1: Short)Long, , method &: (x$1: Short)Long, , method ^: (x$1: Short)Long, , method ==: (x$1: Char)Boolean, , method !=: (x$1: Char)Boolean, , method <: (x$1: Char)Boolean, , method <=: (x$1: Char)Boolean, , method >: (x$1: Char)Boolean, , method >=: (x$1: Char)Boolean, , method +: (x$1: Char)Long, , method -: (x$1: Char)Long, , method *: (x$1: Char)Long, , method /: (x$1: Char)Long, , method %: (x$1: Char)Long, , method |: (x$1: Char)Long, , method &: (x$1: Char)Long, , method ^: (x$1: Char)Long, , method ==: (x$1: Int)Boolean, , method !=: (x$1: Int)Boolean, , method <: (x$1: Int)Boolean, , method <=: (x$1: Int)Boolean, , method >: (x$1: Int)Boolean, , method >=: (x$1: Int)Boolean, , method +: (x$1: Int)Long, , method -: (x$1: Int)Long, , method *: (x$1: Int)Long, , method /: (x$1: Int)Long, , method %: (x$1: Int)Long, , method |: (x$1: Int)Long, , method &: (x$1: Int)Long, , method ^: (x$1: Int)Long, , method ==: (x$1: Long)Boolean, , method !=: (x$1: Long)Boolean, , method <: (x$1: Long)Boolean, , method <=: (x$1: Long)Boolean, , method >: (x$1: Long)Boolean, , method >=: (x$1: Long)Boolean, , method +: (x$1: Long)Long, , method -: (x$1: Long)Long, , method *: (x$1: Long)Long, , method /: (x$1: Long)Long, , method %: (x$1: Long)Long, , method |: (x$1: Long)Long, , method &: (x$1: Long)Long, , method ^: (x$1: Long)Long, , method ==: (x$1: Float)Boolean, , method !=: (x$1: Float)Boolean, , method <: (x$1: Float)Boolean, , method <=: (x$1: Float)Boolean, , method >: (x$1: Float)Boolean, , method >=: (x$1: Float)Boolean, , method +: (x$1: Float)Float, , method -: (x$1: Float)Float, , method *: (x$1: Float)Float, , method /: (x$1: Float)Float, , method %: (x$1: Float)Float, , method ==: (x$1: Double)Boolean, , method !=: (x$1: Double)Boolean, , method <: (x$1: Double)Boolean, , method <=: (x$1: Double)Boolean, , method >: (x$1: Double)Boolean, , method >=: (x$1: Double)Boolean, , method +: (x$1: Double)Double, , method -: (x$1: Double)Double, , method *: (x$1: Double)Double, , method /: (x$1: Double)Double, , method %: (x$1: Double)Double, ),
                          "$plus"),
                        List( // 1 arguments(s)
                          Literal(Constant(1))
                        )
                      )
                    )
                  )
                )
              ),
              DefDef( // sym=method prePut, sym.owner=class TweetMeta, sym.tpe=(model: Any)Unit, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "prePut",
                List(), // no type parameter
                List(
                  List( // 1 parameter(s)
                    ValDef( // sym=value model, sym.owner=method prePut, sym.tpe=Any, tpe=<notype>, tpe.sym=<none>
                      PARAM, // flags=<param>, annots=List()
                      "model",
                      TypeTree(), // sym=class Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala,
                      EmptyTree
                    )
                  )
                ),
                Unit,
                Block( // sym=null, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                  List( // 1 statement(s)
                    Apply( // sym=method assignKeyIfNecessary, tpe=com.google.appengine.api.datastore.Key, tpe.sym=class Key, tpe.sym.owner=package datastore
                      Select( // sym=method assignKeyIfNecessary, sym.owner=class ModelMeta, sym.tpe=(x$1: Any)com.google.appengine.api.datastore.Key, tpe=(x$1: Any)com.google.appengine.api.datastore.Key, tpe.sym=<none>
                        This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                        "assignKeyIfNecessary"),
                      List( // 1 arguments(s)
                        Ident("model") // sym=value model, sym.owner=method prePut, sym.tpe=Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala
                      )
                    )
                  ),
                  Apply( // sym=method incrementVersion, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                    Select( // sym=method incrementVersion, sym.owner=class TweetMeta, sym.tpe=(model: Any)Unit, tpe=(model: Any)Unit, tpe.sym=<none>
                      This("TweetMeta"), // sym=class TweetMeta, sym.owner=package meta, sym.tpe=tutorial.meta.TweetMeta, tpe=TweetMeta.this.type, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                      "incrementVersion"),
                    List( // 1 arguments(s)
                      Ident("model") // sym=value model, sym.owner=method prePut, sym.tpe=Any, tpe=Any, tpe.sym=class Any, tpe.sym.owner=package scala
                    )
                  )
                )
              ),
              DefDef( // sym=method getSchemaVersionName, sym.owner=class TweetMeta, sym.tpe=()java.lang.String, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "getSchemaVersionName",
                List(), // no type parameter
                List(
                ),
                java.lang.String,
                Literal(Constant(slim3.schemaVersion))
              ),
              DefDef( // sym=method getClassHierarchyListName, sym.owner=class TweetMeta, sym.tpe=()java.lang.String, tpe=<notype>, tpe.sym=<none>
                OVERRIDE | METHOD, // flags=override <method>, annots=List()
                "getClassHierarchyListName",
                List(), // no type parameter
                List(
                ),
                java.lang.String,
                Literal(Constant(slim3.classHierarchyList))
              )
            )
          )
        )
    }


    def metaObjectTmpl:Tree = {
      ClassDef( // sym=object TweetMeta, sym.owner=package meta, sym.tpe=object tutorial.meta.TweetMeta, tpe=<notype>, tpe.sym=<none>
        Modifiers( FINAL | MODULE | MODULEVAR | SYNTHETICMETH | MONOMORPHIC ), // flags=final <module>, annots=List()
        "TweetMeta",
        List(), // no type parameter
        Template( // sym=value <local TweetMeta>, sym.owner=object TweetMeta, sym.tpe=<notype>, tpe=object tutorial.meta.TweetMeta, tpe.sym=object TweetMeta, tpe.sym.owner=package meta
          //List(class TweetMeta, trait ScalaObject), // parents
          List(),
          ValDef( // sym=<none>, sym.tpe=<notype>, tpe=<notype>, tpe.sym=<none>
            Modifiers(0), // flags=, annots=List()
            "_",
            TypeTree(), // sym=<none>, tpe=<notype>, tpe.sym=<none>,
            EmptyTree
          ),
          List( // body
            DefDef( // sym=constructor TweetMeta, isPrimaryConstructor, sym.owner=object TweetMeta, sym.tpe=()object tutorial.meta.TweetMeta, tpe=<notype>, tpe.sym=<none>
              METHOD, // flags=<method>, annots=List()
              "<init>",
              List(), // no type parameter
              List(List()), // no parameter
              object tutorial.meta.TweetMeta,
              Block( // sym=null, tpe=Unit, tpe.sym=class Unit, tpe.sym.owner=package scala
                List( // 1 statement(s)
                  Apply( // sym=constructor TweetMeta, tpe=tutorial.meta.TweetMeta, tpe.sym=class TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()tutorial.meta.TweetMeta, , value content: => org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value content: org.slim3.datastore.StringAttributeMeta[tutorial.model.Tweet], , value createdDate: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value createdDate: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,java.util.Date], , value key: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value key: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,com.google.appengine.api.datastore.Key], , value version: => org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , value version: org.slim3.datastore.CoreAttributeMeta[tutorial.model.Tweet,Long], , method entityToModel: (entity: com.google.appengine.api.datastore.Entity)tutorial.model.Tweet, , method modelToEntity: (model: Any)com.google.appengine.api.datastore.Entity, , method getKey: (model: Any)com.google.appengine.api.datastore.Key, , method setKey: (model: Any,key: com.google.appengine.api.datastore.Key)Unit, , method getVersion: (model: Any)Long, , method incrementVersion: (model: Any)Unit, , method prePut: (model: Any)Unit, , method getSchemaVersionName: ()java.lang.String, , method getClassHierarchyListName: ()java.lang.String, )
                    Select( // sym=constructor TweetMeta, isPrimaryConstructor, sym.owner=class TweetMeta, sym.tpe=()tutorial.meta.TweetMeta, tpe=()tutorial.meta.TweetMeta, tpe.sym=<none>
                      Super("", ""), // sym=object TweetMeta, sym.owner=package meta, sym.tpe=object tutorial.meta.TweetMeta, tpe=tutorial.meta.TweetMeta.type, tpe.sym=object TweetMeta, tpe.sym.owner=package meta,
                      "<init>"),
                    Nil // no argument
                  )
                ),
                Literal(Constant(()))
              )
            ),
            DefDef( // sym=method get, sym.owner=object TweetMeta, sym.tpe==> object tutorial.meta.TweetMeta, tpe=<notype>, tpe.sym=<none>
              METHOD, // flags=<method>, annots=List()
              "get",
              List(), // no type parameter
              List(
              ),
              object tutorial.meta.TweetMeta,
              This("") // sym=object TweetMeta, sym.owner=package meta, sym.tpe=object tutorial.meta.TweetMeta, tpe=tutorial.meta.TweetMeta.type, tpe.sym=object TweetMeta, tpe.sym.owner=package meta, tpe.decls=List(constructor TweetMeta: ()object tutorial.meta.TweetMeta, , method get: => object tutorial.meta.TweetMeta, )
            )
          )
        )
      )
    }
*/

  }// end of GenerateTransformer
}// end of ModelMetaTransformComponent
