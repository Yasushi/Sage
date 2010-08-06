package sage

import com.google.appengine.api.datastore._
import scalaz._
import Scalaz._

import scala.collection.JavaConversions._

case class Find[T](base: EntityBase[T], q: Query, fo: FetchOptions) {  
  def query(f: Query => Query): Find[T] = this copy (q = f(q))

  def fetch(f: FetchOptions => FetchOptions): Find[T] = this copy (fo = f(fo))

  def iterable(implicit ds: DatastoreService): Iterable[Keyed[T]] = 
    ds.prepare(q).asIterable(fo) flatMap (e => base.read(e) map (t => Keyed(e.getKey, t)))

  def keys(implicit ds: DatastoreService): Iterable[Key] =
    ds.prepare(q.setKeysOnly).asIterable(fo) map (_.getKey)
}

private[sage] object Find {
  private[sage] def defaultOptions = FetchOptions.Builder.withChunkSize(FetchOptions.DEFAULT_CHUNK_SIZE)

  def apply[T](base: EntityBase[T]): Find[T] = Find(base, new Query(base.kind), defaultOptions)
}

trait FindDSL {
  val field: String
  type QRY = Query => Query

  import com.google.appengine.api.datastore.Query.SortDirection._
  import com.google.appengine.api.datastore.Query.FilterOperator._

  def asc: QRY = (_.addSort(field, ASCENDING))
  def desc: QRY = (_.addSort(field, DESCENDING))
  def ?==[T](t: T): QRY = (_.addFilter(field, EQUAL, t))

  def ?!=[T](t: T): QRY = (_.addFilter(field, NOT_EQUAL, t))

  def ?>[T](t: T): QRY = (_.addFilter(field, GREATER_THAN, t))
  def ?>=[T](t: T): QRY = (_.addFilter(field, GREATER_THAN_OR_EQUAL, t))
  def ?<[T](t: T): QRY = (_.addFilter(field, LESS_THAN, t))
  def ?<=[T](t: T): QRY = (_.addFilter(field, LESS_THAN_OR_EQUAL, t))
  def ?⊂[T <: Iterable[_]](t: T): QRY = (_.addFilter(field, IN, scala.collection.JavaConversions.asIterable(t)))
}

trait FindDSLImplicits {
  implicit def stringTo(s: String): FindDSL = new FindDSL { val field = s }
  implicit def stringFrom(dsl: FindDSL): String = dsl.field
}

object dsl extends FindDSLImplicits
