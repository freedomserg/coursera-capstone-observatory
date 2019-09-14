package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}

import scala.math.BigDecimal.RoundingMode.HALF_UP
import scala.math.{Pi, acos, cos, pow, round, sin}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EARTH_RADIUS = 6372.8

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location     Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    temperatures.find { locTempPair =>
      val knownLocation = locTempPair._1
      distance(location, knownLocation) <= 1000
    } match {
      case Some((_, temp)) => temp
      case None =>
        val (numer, denom) = temperatures.foldLeft((0.0, 0.0)) { case (res, (thisLoc, thisTemp)) =>
          val w = weight(location, thisLoc)
          val updatedNumer = res._1 + w * thisTemp
          val updatedDenom = res._2 + w
          (updatedNumer, updatedDenom)
        }
        numer / denom
    }
  }

  private def weight(location1: Location, location2: Location): Double = {
    val power = 3
    val dist = distance(location1, location2)
    1 / pow(dist, power)
  }

  //in meters
  def distance(location1: Location, location2: Location): Double = {
    val lat1 = location1.lat.toRadians
    val lat2 = location2.lat.toRadians

    val lonDelta = (location2.lon - location1.lon).toRadians
    val latDelta = (location2.lat - location1.lat).toRadians


    val angleDelta =
        (location1, location2) match {
          case _ if equalPoints(location1, location2) => 0
          case _ if antipodes(location1, location2) => Pi
          case _ => acos(sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lonDelta))
        }

      EARTH_RADIUS * angleDelta * 1000
  }

  private def equalPoints(location1: Location, location2: Location): Boolean = location1.lon == location2.lon && location1.lat == location2.lat

  private def antipodes(location1: Location, location2: Location): Boolean = {
    val antipodeLon1 = location1.lon match {
      case lon if lon > 0 => scala.math.abs(location1.lon) - 180
      case lon if lon <= 0 => scala.math.abs(location1.lon) + 180
    }
    val antipodeLat1 = location1.lat * (-1)
    location2.lon == antipodeLon1 && location2.lat == antipodeLat1
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value  The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toList.sortBy(pair => pair._1)

    if (sortedPoints.head._1 >= value) sortedPoints.head._2
    else if (sortedPoints.last._1 <= value) sortedPoints.last._2
    else {
      val upperPoint = sortedPoints(sortedPoints.indexWhere(_._1 >= value))
      val lowerPoint = sortedPoints(sortedPoints.lastIndexWhere(_._1 <= value))

      val upperRed = upperPoint._2.red
      val upperGreen = upperPoint._2.green
      val upperBlue = upperPoint._2.blue
      val lowerRed = lowerPoint._2.red
      val lowerGreen = lowerPoint._2.green
      val lowerBlue = lowerPoint._2.blue

      val upperTemp = upperPoint._1
      val lowerTemp = lowerPoint._1


      val targetRed = round((lowerRed * (upperTemp - value) + upperRed * (value - lowerTemp)) / (upperTemp - lowerTemp)).toInt
      val targetGreen = round((lowerGreen * (upperTemp - value) + upperGreen * (value - lowerTemp)) / (upperTemp - lowerTemp)).toInt
      val targetBlue = round((lowerBlue * (upperTemp - value) + upperBlue * (value - lowerTemp)) / (upperTemp - lowerTemp)).toInt

      Color(targetRed, targetGreen, targetBlue)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    val (width, height) = (360, 180)
    val pixels: Array[Pixel] = new Array[Pixel](width * height)

    for {
      w <- 0 until width
      h <- 0 until height
    } yield {
      val (lat, lon) = (-h + 90, w - 180)
      val temp: Temperature = predictTemperature(temperatures, Location(lat, lon))
      val color: Color = interpolateColor(colors, temp)

      val pixel: Pixel = Pixel(RGBColor(color.red, color.green, color.blue))
      pixels(h * width + w) = pixel
    }

    Image(width, height, pixels)
  }


}

