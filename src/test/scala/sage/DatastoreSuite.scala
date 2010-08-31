package sage

import com.google.appengine.api.datastore._
import com.google.appengine.tools.development.testing._
import dev.LocalDatastoreService

import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

trait SageSuiteBase extends FunSuite with ShouldMatchers with BeforeAndAfterAll with DatastoreSuite

trait DatastoreSuite {
  self: BeforeAndAfterAll =>

  val helper = new LocalServiceTestHelper(
    new LocalDatastoreServiceTestConfig().setBackingStoreLocation(".").setNoStorage(true)
  )

  implicit var datastoreService: DatastoreService = DatastoreServiceFactory.getDatastoreService

  override def beforeAll {
    helper.setUp
  }

  override def afterAll {
    helper.tearDown
  }
}
