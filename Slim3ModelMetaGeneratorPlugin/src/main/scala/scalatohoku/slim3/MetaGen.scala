package scalatohoku.slim3

class MetaGen(val packageName: String, val modelName: String) {
  val props = new scala.collection.mutable.HashMap[String, String] // name -> className
  val annotations = new collection.mutable.HashMap[String, List[String]]

  def generateSourceFile() {
    import java.io._
    val metaSrcDir = "src/main/scala/" + packageName.replace('.', '/') + "/meta"
    val dir = new File(metaSrcDir)
    if (!dir.exists) dir.mkdirs
    val file = new FileWriter(metaSrcDir + "/" + modelName + "Meta.scala")
    file.write(contents)
    file.close
  }

  private def attribute(prop: String, k: String): String = {
    annotations.get(prop).flatMap {
      _.find(_.startsWith("org.slim3.datastore.Attribute")).flatMap { a =>
        System.out.println(a)
        val re = """.*%s = ([^,]+).*\)""".format(k).r
        a match {
          case re(v) => Some(v.trim)
          case _ => None
        }
      }
    } getOrElse ""
  }

  private def contents: String = {
    val collections = collection.mutable.Map[String, Pair[String, String]]()
    val persistentProps = props.keys.filter { attribute(_, "persistent") != "false" }
    val CollectionType = """java\.util\.(?:ArrayList|LinkedList|HashSet|LinkedHashSet|TreeSet|List|Set|SortedSet)\[(.+)\]""".r

    val valsSrc = persistentProps.map(k => props(k) match {
      case "String" =>
        val unindexed = if (attribute(k, "unindexed") == "true") "Unindexed" else ""
        MetaTemplate.Attr.String.replace("$$propname$$", k).replace("$$unindexed$$", unindexed)
      case "com.google.appengine.api.datastore.Key" =>
        MetaTemplate.Attr.Key
      case clazz @ CollectionType(t) =>
        val unindexed = if (attribute(k, "unindexed") == "true") "Unindexed" else ""
        collections += (k -> (clazz, t))
        if (t == "String")
          MetaTemplate.Attr.StringCollection.replace("$$propname$$", k).replace("$$typename$$", clazz).replace("$$unindexed$$", unindexed)
        else
          MetaTemplate.Attr.Collection.replace("$$propname$$", k).replace("$$typename$$", clazz).replace("$$typename2$$", t).replace("$$unindexed$$", unindexed)
      case clazz =>
        val unindexed = if (attribute(k, "unindexed") == "true") "Unindexed" else ""
        MetaTemplate.Attr.Core.replace("$$propname$$", k).replace("$$typename$$", clazz).replace("$$unindexed$$", unindexed)
    })

    var prePutSrc = ""
    var staticSrc = ""
    props.keys.foreach { k =>
      val re = """classOf\[(.+)\]""".r
      attribute(k, "listener") match {
        case re(v) =>
          if (prePutSrc == "") prePutSrc = MetaTemplate.PrePutHeader
          prePutSrc += MetaTemplate.PrePut.replace("$$modelname$$", modelName).replace("$$propname$$", k).replace("$$getter$$", toGetter(k)).replace("$$setter$$", toSetter(k))
          staticSrc += MetaTemplate.AttributeListener.replace("$$propname$$", k).replace("$$listener$$", v)
        case e =>
          println("no listener found: " + e)
      }
    }

    val modelToEntitySrc = persistentProps.map(k =>
      (props(k) match {
        case "com.google.appengine.api.datastore.Text" | "com.google.appengine.api.datastore.Blob" =>
          MetaTemplate.ModelToEntity.Unindexed
        case "String" if attribute(k, "lob") == "true" => MetaTemplate.ModelToEntity.LongText
        case "Array[Byte]" if attribute(k, "lob") == "true" => MetaTemplate.ModelToEntity.Blob
        case _ if attribute(k, "unindexed") == "true" => MetaTemplate.ModelToEntity.Unindexed
        case _ => MetaTemplate.ModelToEntity.Indexed
      }).replace("$$propname$$", k).replace("$$getter$$", toGetter(k)))

    val entityToModelSrc = persistentProps.map { k =>
      if (attribute(k, "lob") != "true")
        MetaTemplate.EntityToModel.replace("$$propname$$", k).replace("$$setter$$", toSetter(k)).replace("$$typename$$", props(k))
      else props(k) match {
        case "String" => MetaTemplate.EntityToModelLongText.replace("$$propname$$", k).replace("$$setter$$", toSetter(k))
        case "Array[Byte]" => MetaTemplate.EntityToModelBlob.replace("$$propname$$", k).replace("$$setter$$", toSetter(k))
        case _ => MetaTemplate.EntityToModelLob.replace("$$propname$$", k).replace("$$setter$$", toSetter(k))
      }
    }

    val modelToJsonSrc = props.keys.map { k =>
      if (collections.contains(k))
        MetaTemplate.ModelToJsonCollection.replace("$$propname$$", k).replace("$$getter$$", toGetter(k))
      else props(k) match {
        case "Array[Byte]" => MetaTemplate.ModelToJsonBlob.replace("$$propname$$", k).replace("$$getter$$", toGetter(k))
        case _ => MetaTemplate.ModelToJson.replace("$$propname$$", k).replace("$$getter$$", toGetter(k))
      }
    }

    val jsonToModelSrc = props.keys.map { k =>
      if (collections.contains(k)) {
        val (col, clz) = collections(k) match {
          case (c, t) if c.startsWith("java.util.List") => ("java.util.ArrayList[%s]".format(t), t)
          case (c, t) if c.startsWith("java.util.Set") => ("java.util.HashSet[%s]".format(t), t)
          case (c, t) if c.startsWith("java.util.SortedSet") => ("java.util.TreeSet[%s]".format(t), t)
          case e => e
        }
        MetaTemplate.JsonToModelCollection.replace("$$propname$$", k).replace("$$clazz$$", clz).replace("$$collection$$", col).replace("$$getter$$", toGetter(k)).replace("$$setter$$", toSetter(k))
      } else props(k) match {
        case "Array[Byte]" =>
          MetaTemplate.JsonToModelBlob.replace("$$propname$$", k).replace("$$clazz$$", props(k)).replace("$$getter$$", toGetter(k)).replace("$$setter$$", toSetter(k))
        case _ =>
          MetaTemplate.JsonToModel.replace("$$propname$$", k).replace("$$clazz$$", props(k)).replace("$$getter$$", toGetter(k)).replace("$$setter$$", toSetter(k))
      }
    }

    MetaTemplate.MetaClass
      .replace("$$instance_variables$$", valsSrc.mkString)
      .replace("$$entity_to_model$$", entityToModelSrc.mkString)
      .replace("$$model_to_entity$$", modelToEntitySrc.mkString)
      .replace("$$model_to_json$$", modelToJsonSrc.mkString)
      .replace("$$json_to_model$$", jsonToModelSrc.mkString)
      .replace("$$pre_put$$", prePutSrc)
      .replace("$$modelname$$", modelName)
      .replace("$$package$$", packageName)
      .replace("$$static$$", staticSrc)
  }

  private def toGetter(s: String) = "get" + toCamelCase(s)
  private def toSetter(s: String) = "set" + toCamelCase(s)
  private def toCamelCase(s: String) = s.substring(0, 1).toUpperCase() + s.substring(1)

}