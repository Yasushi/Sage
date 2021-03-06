package sage

import hprops._
import scalaz._
import Scalaz._
import com.google.appengine.api.datastore._
import scala.collection.JavaConversions._

trait EntityBase[T] {
  val kind: String
  def * : ReadWrite[Entity, T]
  
  def <<(t: T)(implicit ds: DatastoreService): Keyed[T] = {
    val e = freshEntity(t)
    Keyed(ds.put(e), t)
  }
  
  def parentedSave(t: T, parent: Key)(implicit ds: DatastoreService): Keyed[T] = {
    val e = parentedEntity(t, parent)
    Keyed(ds.put(e), t)
  }
  
  def <<(kt: Keyed[T])(implicit ds: DatastoreService): Keyed[T] = {
    val e = write(kt.value, new Entity(kt.key))
    Keyed(ds.put(e), kt.value)
  }
  
  def <<++(ts: Seq[T])(implicit ds: DatastoreService): Iterable[Keyed[T]] = {
    val es = ts map freshEntity
    val keys: Iterable[Key] = ds.put(asIterable(es))
    keys zip ts map ((k:Key, b:T) => Keyed(k,b)).tupled
  }
  
  def keyedSave(kts: Seq[Keyed[T]])(implicit ds: DatastoreService): Iterable[Keyed[T]] = {
    val es = kts map (kt => write(kt.value, new Entity(kt.key)))
    val keys: Iterable[Key] = ds.put(asIterable(es))
    keys zip kts map ((k:Key, b:Keyed[T]) => Keyed(k,b.value)).tupled
  }

  def lookup(id: Long)(implicit ds: DatastoreService): Option[Keyed[T]] = {
    val got = (() => ds.get(KeyFactory.createKey(kind, id))).throws.toOption
    for (entity <- got; t <- read(entity)) yield (Keyed(entity.getKey, t))
  }

  def lookup(key: Key)(implicit ds: DatastoreService): Option[Keyed[T]] = {
    val got = (() => ds.get(key)).throws.toOption
    for (entity <- got; t <- read(entity)) yield (Keyed(entity.getKey, t))
  }
  
  def childrenOf(pk: Key)(implicit ds: DatastoreService): Iterable[Keyed[T]] = {
    find.query(qry => qry.setAncestor(pk)).iterable
  }
  
  def find: Find[T] = Find(this)
  
  def write(t: T, e: Entity): Entity = this.*.put(t, e).toOption.get
  def read(m: Entity): Option[T] = this.*.read(m).toOption
  
  private def keyedEntity(t: T, key: Key): Entity = write(t, new Entity(key))
  private def freshEntity(t: T) = write(t, new Entity(kind))  
  private def parentedEntity(t: T, parentKey: Key) = write(t, new Entity(kind, parentKey))  
}

abstract class Base[T](val kind: String) extends EntityBase[T]
