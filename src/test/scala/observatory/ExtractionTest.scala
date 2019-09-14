package observatory

import java.time.LocalDate

import org.junit.runner.RunWith
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
trait ExtractionTest extends FunSuite with BeforeAndAfterAll {

  def initializeExtraction(): Boolean =
    try {
      Extraction
      true
    } catch {
      case ex: Throwable =>
        println(ex.getMessage)
        ex.printStackTrace()
        false
    }

  override def afterAll(): Unit = {
    assert(initializeExtraction(), " -- is your Extraction object OK ?")
    import Extraction._
    spark.stop()
  }

  test("'locateTemperatures' returns a Seq of 4 entries with proper Celsius temperatures retaining items with STN or WBAN empty") {
    assert(initializeExtraction(), " -- is your Extraction object OK ?")
    import Extraction._

    val expected = Seq(
      (LocalDate.of(1975, 1, 1), Location(+24.640, -082.860), 10.0),
      (LocalDate.of(1975, 1, 2), Location(+29.140, -083.030), 15.56),
      (LocalDate.of(1975, 1, 3), Location(+62.737, -141.208), 21.11),
      (LocalDate.of(1975, 1, 3), Location(+64.502, -154.130), 21.11)
    )
    val result = locateTemperatures(1975, "/stations1975.csv", "/1975.csv")
    assert(result.toSeq == expected)
  }

  test("'locateTemperatures' returns a Seq of 2 entries with proper Celsius for 1976 ignoring stations with no location coordinates") {
    assert(initializeExtraction(), " -- is your Extraction object OK ?")
    import Extraction._

    val expected = Seq(
      (LocalDate.of(1976, 1, 1), Location(+62.737,-141.208), 10.0),
      (LocalDate.of(1976, 1, 2), Location(+64.502,-154.130), 15.56)
    )
    val result = locateTemperatures(1976, "/stations1976.csv", "/1976.csv")
    assert(result.toSeq == expected)
  }

  test("'locationYearlyAverageRecords' returns a proper average temperature per location for 1976") {
    assert(initializeExtraction(), " -- is your Extraction object OK ?")
    import Extraction._

    val input = Seq(
      (LocalDate.of(1976, 1, 1), Location(+62.737,-141.208), 10.0),
      (LocalDate.of(1976, 1, 2), Location(+62.737,-141.208), 15.56),
      (LocalDate.of(1976, 1, 1), Location(+64.502,-154.130), 15.56),
      (LocalDate.of(1976, 1, 2), Location(+64.502,-154.130), 21.11)
    )

    val expected = Seq(
      (Location(+62.737,-141.208), 12.78),
      (Location(+64.502,-154.130), 18.34)
    )

    val result = locationYearlyAverageRecords(input)
    assert(result.toSeq == expected)
  }
}