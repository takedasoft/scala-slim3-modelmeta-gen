/**
 * Scala版　Slim3モデルのサンプル。
 * 各プロパティのアクセサを生成するために、@BeanPropertyアノテーションを付けます。
 */
package tutorial.model

import java.util.Date
import scala.reflect.BeanProperty

import com.google.appengine.api.datastore.Key
import org.slim3.datastore.Attribute
import org.slim3.datastore.Model

@Model(schemaVersion = 1)
class Tweet {
    @Attribute(primaryKey = true)
    @BeanProperty
    var key:Key = _
    
    @Attribute(version = true)
    @BeanProperty
    var version:Long = _
   
    @BeanProperty
    var content:String = _
   
    @BeanProperty
    var createdDate:Date = new Date
}
