package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import observatory.Visualization.{interpolateColor, predictTemperature}

import scala.math.{Pi, atan, sinh}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val lat = atan(sinh(Pi * (1.0 - 2.0 * tile.y.toDouble / (1 << tile.zoom)))).toDegrees
    val lon = tile.x.toDouble / (1 << tile.zoom) * 360.0 - 180.0
    Location(lat, lon)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    def generatePixel(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Pixel = {
      val tileLoc = tileLocation(tile)
      val temp: Temperature = predictTemperature(temperatures, tileLoc)
      val color: Color = interpolateColor(colors, temp)
      Pixel(RGBColor(color.red, color.green, color.blue, 127))
    }

    val (width, height) = (256, 256)

    val subTiles =  for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      Tile(tile.x + x, tile.y + y, tile.zoom + 8)
      //Tile(tile.x * width + x, tile.y * height + y, tile.zoom + 8)
    }

    val pixels = subTiles.par.map(st => generatePixel(temperatures, colors, st)).toArray

    Image(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    //val tuples: Iterable[(Location, Temperature)] = Extraction.locationYearlyAverageRecords()
    yearlyData.par.foreach { case (year, data) =>
      for {
        zoom <- 0 to 3
        x <- 0 until (1 << zoom)
        y <- 0 until (1 << zoom)
      } yield generateImage(year, Tile(x, y, zoom), data)
    }
  }

//  private def generateImageInternal(year: Year, t: Tile, data: Iterable[(Location, Temperature)]): Unit = {
//    val colors = Seq(
//      (60.0, Color(255, 255, 255)),
//      (32.0, Color(255, 0, 0)),
//      (12.0, Color(255, 255, 0)),
//      (0.0, Color(0, 255, 255)),
//      (-15.0, Color(0, 0, 255)),
//      (-27.0, Color(255, 0, 255)),
//      (-50.0, Color(33, 0, 107)),
//      (-60.0, Color(0, 0, 0))
//    )
//    val image = tile(data, colors, t)
//  }

}
