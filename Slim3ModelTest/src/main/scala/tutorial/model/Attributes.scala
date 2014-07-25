/**
 Attribute annotationのテスト
 @see https://sites.google.com/site/slim3documentja/documents/slim3-datastore/defining-data-classes
 */
package tutorial.model

import java.util.Date
import scala.reflect.BeanProperty

import com.google.appengine.api.datastore.Key
import org.slim3.datastore._

@Model(schemaVersion = 1)
class Attributes {
    @Attribute(primaryKey = true)
    @BeanProperty
    var key:Key = _
    
    @Attribute(version = true)
    @BeanProperty
    var version:Long = _
   
    @BeanProperty
    @Attribute(unindexed = false)
    var indexedString:String = _
   
    @BeanProperty
    @Attribute(unindexed = true)
    var unindexedString:String = _
    
    @BeanProperty
    @Attribute(unindexed = true)
    var unindexedCore:Date = new Date
    
    @BeanProperty
    @Attribute(listener = classOf[ModificationDate])
    var updated:Date = _
    
    @BeanProperty
    @Attribute(listener = classOf[CreationDate])
    var created:Date = _
    
    @BeanProperty
    @Attribute(persistent = false)
    var nonpersistent: String = _
}
