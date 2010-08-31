package sage

import scalaz._
import Scalaz._

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

import com.google.appengine.api.datastore._

import java.lang.{Long => JLong}

class FindSuite extends SageSuiteBase {
  
  case class Hat(name: String, price: JLong)
  
  object Hats extends Base[Hat]("hats") {
    def * =  "type".prop[String] :: "price".prop[JLong]  >< (Hat <-> Hat.unapply _)
  }
  
  test("find something") {
    import dsl._
    val newHats = List(Hat("a", 1), Hat("b", 2))
    val keys = Hats <<++ newHats map (_.key)
    
    val hatsNamedA = Hats.find.query("type" ?== "a").iterable
    
    hatsNamedA map (_.value) should equal (List(Hat("a", 1)))

    (Hats.find.query("price" ?> 1).iterable.map(_.value)) should equal (List(Hat("b", 2)))
    (Hats.find.query("price" ?>= 2).iterable.map(_.value)) should equal (List(Hat("b", 2)))
    (Hats.find.query("price" ?> 2).iterable.map(_.value)) should equal (List())
    (Hats.find.query("price" ?< 2).iterable.map(_.value)) should equal (List(Hat("a", 1)))
    (Hats.find.query("price" ?<= 1).iterable.map(_.value)) should equal (List(Hat("a", 1)))
    (Hats.find.query("price" ?⊂ List(1)).iterable.map(_.value)) should equal (List(Hat("a", 1)))
  }
}
