package scalatohoku.slim3

object MetaTemplate {
  object Attr {
    val String = """
  val $$propname$$ = new StringAttributeMeta[$$modelname$$](this, "$$propname$$", "$$propname$$")"""
    val Core = """
  val $$propname$$ = new CoreAttributeMeta[$$modelname$$,$$typename$$](this, "$$propname$$", "$$propname$$",classOf[$$typename$$])"""
    val Key = """
  val key = new CoreAttributeMeta[$$modelname$$,Key](this, "__key__", "key", classOf[Key])"""
  }
  val EntityToModel = """
    model.$$setter$$(entity.getProperty("$$propname$$").asInstanceOf[$$typename$$])"""

  object ModelToEntity {
    val Indexed = """
    entity.setProperty("$$propname$$", m.$$getter$$())"""
    val Unindexed = """
    entity.setUnindexedProperty("$$propname$$", m.$$getter$$())"""
  }
  
  val MetaClass = """// auto generated by ModelMetaGeneratorPlugin
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
}