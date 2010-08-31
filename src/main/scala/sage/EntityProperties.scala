package sage

import hprops._
import scalaz._
import Scalaz._
import com.google.appengine.api.datastore._

object EntityPropBuilders {
  def string[T : ClassManifest](s: String): ReadWrite[Entity, T] = new ReadWrite[Entity, T] {
    def read(e: Entity) = e.property[T](s).toSuccess(missing(s).wrapNel)
    
    def put(t: T, e: Entity) = {
      e.setProperty(s, t)
      success(e)
    }
  }

  def optional[T : ClassManifest](s: String): ReadWrite[Entity, Option[T]] = new ReadWrite[Entity, Option[T]] {
    def read(e: Entity) = success(e.property[T](s))
    def put(t: Option[T], e: Entity) = {
      e.setProperty(s, t.getOrElse(null))
      success(e)
    }
  }

  def stringNoIndex[T : ClassManifest](s: String): ReadWrite[Entity, T] = new ReadWrite[Entity, T] {
    def read(e: Entity) = e.property[T](s).toSuccess(missing(s).wrapNel)

    def put(t: T, e: Entity) = {
      e.setUnindexedProperty(s, t)
      success(e)
    }
  }

  def optionalNoIndex[T : ClassManifest](s: String): ReadWrite[Entity, Option[T]] = new ReadWrite[Entity, Option[T]] {
    def read(e: Entity) = success(e.property[T](s))
    def put(t: Option[T], e: Entity) = {
      e.setUnindexedProperty(s, t.getOrElse(null))
      success(e)
    }
  }
  
  def newType[T, U <: NewType[T]](fieldName: String, f: T => U)(implicit m: ClassManifest[T]) = 
    string[T](fieldName) >< (f, ((_: NewType[T]).value))
}

trait StringW {
  val s: String
  def typedProp[T, U <: NewType[T]](f: T => U)(implicit m: ClassManifest[T]): ReadWrite[Entity, U] = 
    EntityPropBuilders.newType[T, U](s, f)
  
  def prop[T : ClassManifest] = EntityPropBuilders.string[T](s) 
  def optProp[T : ClassManifest] = EntityPropBuilders.optional[T](s)
  def propNi[T : ClassManifest] = EntityPropBuilders.stringNoIndex[T](s)
  def optPropNi[T : ClassManifest] = EntityPropBuilders.optionalNoIndex[T](s)
}

trait NewTypeConsW[T, U <: NewType[T]] {
  val f: T => U
  
  def prop(implicit mu: ClassManifest[U], mt: ClassManifest[T]) = {
    val thisName = mu.erasure.getSimpleName.toLowerCase
    EntityPropBuilders.newType[T, U](thisName, f)
  }
}

trait EntityProperties {
  implicit def newTypeConsProp[T, U <: NewType[T]](g: T => U) = new NewTypeConsW[T, U] {
    val f = g
  }  
  
  implicit def stringProp(str: String) = new StringW { val s = str }   
}
