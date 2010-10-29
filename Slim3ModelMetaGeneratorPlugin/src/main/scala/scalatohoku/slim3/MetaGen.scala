package scalatohoku.slim3

class MetaGen(val packageName:String,val modelName:String) {
  val props = new scala.collection.mutable.HashMap[String,String] // name -> className

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
        MetaTemplate.Attr.String.replace("$$propname$$",k)
      case "com.google.appengine.api.datastore.Key" =>
        MetaTemplate.Attr.Key
      case clazz =>
        MetaTemplate.Attr.Core.replace("$$propname$$",k).replace("$$typename$$", clazz)
    } )
    val modelToEntitySrc  = props.keys.map( k => 
      ( props(k) match {
        case "com.google.appengine.api.datastore.Text" | "com.google.appengine.api.datastore.Blob" =>
          MetaTemplate.ModelToEntity.Unindexed
        case _  => MetaTemplate.ModelToEntity.Indexed
      }).replace("$$propname$$",k).replace("$$getter$$",toGetter(k))
    )

    val entityToModelSrc  = props.keys.map( k =>
      MetaTemplate.EntityToModel.replace("$$propname$$",k).replace("$$setter$$",toSetter(k)).replace("$$typename$$", props(k))
    )
    MetaTemplate.MetaClass
    .replace("$$instance_variables$$",valsSrc.mkString)
    .replace("$$entity_to_model$$",entityToModelSrc.mkString)
    .replace("$$model_to_entity$$",modelToEntitySrc.mkString)
    .replace("$$modelname$$",modelName)
    .replace("$$package$$",packageName)
  }

  private def toGetter(s:String) = "get" + toCamelCase(s)
  private def toSetter(s:String) = "set" + toCamelCase(s)
  private def toCamelCase(s:String) = s.substring(0, 1).toUpperCase() + s.substring(1)

}