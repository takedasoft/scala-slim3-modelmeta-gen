/**
 全てのデータ型のテスト
 @see http://sites.google.com/site/slim3appengine/slim3-datastore/defining-data-classes/core-value-types
 */
package tutorial.model

import java.util.Date
import scala.reflect.BeanProperty

import com.google.appengine.api.datastore.Key
import org.slim3.datastore.Attribute
import org.slim3.datastore.Model

@Model(schemaVersion = 1)
class AllDataType {

    @Attribute(primaryKey = true)
    @BeanProperty
    var key:Key = _

    @Attribute(version = true)
    @BeanProperty
    var version:Long = _

    @BeanProperty
    var name:String = _
    
    @BeanProperty
    var blob:com.google.appengine.api.datastore.ShortBlob = _
    
    @BeanProperty
    var bool:Boolean = _

    @BeanProperty
    var num1:Short = _

    @BeanProperty
    var num2:Int = _

    @BeanProperty
    var num3:Long = _

    @BeanProperty
    var num4:Float = _

    @BeanProperty
    var num5:Double = _

    @BeanProperty
    var date:java.util.Date = _

    @BeanProperty
    var guser:com.google.appengine.api.users.User = _

    @BeanProperty
    var longText:com.google.appengine.api.datastore.Text = _ //Not indexed.

    @BeanProperty
    var longBinary:com.google.appengine.api.datastore.Blob = _ //Not indexed.

    @BeanProperty
    var category:com.google.appengine.api.datastore.Category = _

    @BeanProperty
    var email:com.google.appengine.api.datastore.Email = _

    @BeanProperty
    var geo:com.google.appengine.api.datastore.GeoPt = _

    @BeanProperty
    var im:com.google.appengine.api.datastore.IMHandle = _

    @BeanProperty
    var url:com.google.appengine.api.datastore.Link = _

    @BeanProperty
    var phone:com.google.appengine.api.datastore.PhoneNumber = _

    @BeanProperty
    var address:com.google.appengine.api.datastore.PostalAddress = _

    @BeanProperty
    var rate:com.google.appengine.api.datastore.Rating = _

}