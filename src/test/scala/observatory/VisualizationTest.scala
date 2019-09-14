package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Visualization._

//@RunWith(classOf[JUnitRunner])
trait VisualizationTest extends FunSuite with Checkers {

  test("Calculate distance Kyiv-Odesa properly") {

    val kyiv = Location(50.45466, 30.5238)
    val odesa = Location(46.4825, 30.7233)
    val expectedDistance = 441

    val result = distance(kyiv, odesa)
    println(s"Calculated distance Kyiv - Odesa: $result")

    assert(scala.math.abs(result - expectedDistance) / expectedDistance <= 0.01, s"Calculated distance $result differs more that 1% over the expected distance $expectedDistance")

  }

  test("Calculate distance Odesa-Kyiv properly") {

    val kyiv = Location(50.45466, 30.5238)
    val odesa = Location(46.4825, 30.7233)
    val expectedDistance = 441

    val result = distance(odesa, kyiv)
    println(s"Calculated distance Odesa - Kyiv: $result")

    assert(scala.math.abs(result - expectedDistance) / expectedDistance <= 0.01, s"Calculated distance $result differs more that 1% over the expected distance $expectedDistance")

  }

  test("Distance is 0 between equal points") {

    val point = Location(50.45466, 30.5238)
    val expectedDistance = 0

    val result = distance(point, point)
    println(s"Calculated distance to the same point: $result")

    assert(result === expectedDistance)

  }

  test("Calculate distance between antipodes properly") {

    val newZeland = Location(-43.532054, 172.636225)
    val alfozSpain = Location(43.532054, -7.363775)
    val expectedDistance = EARTH_RADIUS * scala.math.Pi

    val result = distance(newZeland, alfozSpain)
    println(s"Expected distance between antipodes New Zeland and Alfoz (Spain): $result")
    println(s"Calculated distance between antipodes New Zeland and Alfoz (Spain): $result")

    assert(scala.math.abs(result - expectedDistance) / expectedDistance <= 0.01, s"Calculated distance $result differs more that 1% over the expected distance $expectedDistance")

  }

  ignore("Color(255, 255, 255) for temperature 61") {
    val points: Iterable[(Temperature, Color)] = Seq((1, Color(1, 255, 255)), (10, Color(10, 255, 255)))
    val expectedColor = Color(255, 255, 255)

    val result = interpolateColor(points, 61)
    println(s"Calculated color: $result")

    assert(result == expectedColor, s"Calculated color for temp 61 should be Color(255, 255, 255)")
  }

  ignore("Color(0, 0, 0) for temperature -61") {
    val points: Iterable[(Temperature, Color)] = Seq((1, Color(1, 255, 255)), (10, Color(10, 255, 255)))
    val expectedColor = Color(0, 0, 0)

    val result = interpolateColor(points, -61)
    println(s"Calculated color: $result")

    assert(result == expectedColor, s"Calculated color for temp -61 should be Color(0, 0, 0)")
  }

  test("Proper color for temperature 20") {
    val points: Iterable[(Temperature, Color)] = Seq((10, Color(10, 250, 250)), (-1, Color(0, 254, 255)), (-5, Color(0, 250, 255)), (30, Color(30, 255, 255)), (45, Color(45, 255, 255)))
    /*val targetRed = BigDecimal(( lowerRed * (upperTemp - value) + upperRed * (value - lowerTemp) ) / (upperTemp - lowerTemp)).setScale(0, HALF_UP).toInt
    targetRed = (10 * (30 - 20) + 30 * (20 - 10)) / (30 - 10) = (10 * 10 + 30 * 10) / 20 = 400 / 20 = 20
        val targetGreen = BigDecimal(( lowerGreen * (upperTemp - value) + upperGreen * (value - lowerTemp) ) / (upperTemp - lowerTemp)).setScale(0, HALF_UP).toInt
        targetGreen = (250 * (30 - 20) + 255 * (20 - 10)) / (30 - 10) = (250 * 10 + 255 * 10) / 20 = (2500 + 2550) / 20 = 5050 / 20 = 252.5 ~ 253
        val targetBlue = BigDecimal(( lowerBlue * (upperTemp - value) + upperBlue * (value - lowerTemp) ) / (upperTemp - lowerTemp)).setScale(0, HALF_UP).toInt
        targetBlue = (250 * (30 - 20) + 255 * (20 - 10)) / (30 - 10) = (250 * 10 + 255 * 10) / 20 = (2500 + 2550) / 20 = 5050 / 20 = 252.5 ~ 253

        Color(20, 253, 253)*/

    val expectedColor = Color(20, 253, 253)

    val result = interpolateColor(points, 20)
    println(s"Calculated color: $result")

    assert(result == expectedColor, s"Calculated color for temp 20 should be Color(20, 253, 253)")
  }


}
