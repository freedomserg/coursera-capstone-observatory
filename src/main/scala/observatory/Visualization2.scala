package observatory

import com.sksamuel.scrimage.{Image, Pixel, RGBColor}
import observatory.Interaction.tileLocation
import observatory.Visualization.interpolateColor

import scala.math.{ceil, floor}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00   Top-left value
    * @param d01   Bottom-left value
    * @param d10   Top-right value
    * @param d11   Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
                             point: CellPoint,
                             d00: Temperature,
                             d01: Temperature,
                             d10: Temperature,
                             d11: Temperature
                           ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) +
      d10 * point.x * (1 - point.y) +
      d01 * (1 - point.x) * point.y +
      d11 * point.x * point.y
  }

  /**
    * @param grid   Grid to visualize
    * @param colors Color scale to use
    * @param tile   Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
                     grid: GridLocation => Temperature,
                     colors: Iterable[(Temperature, Color)],
                     tile: Tile
                   ): Image = {

    val (width, height) = (256, 256)
    val pixelLocations = for {
      x <- 0 until width
      y <- 0 until height
    } yield {
      val subtile = Tile(tile.x + x, tile.y + y, tile.zoom + 8)
      tileLocation(subtile)
    }

    val pixels = pixelLocations.par
      .map { pixelLoc =>

        val latRange = List(floor(pixelLoc.lat).toInt, ceil(pixelLoc.lat).toInt)
        val lonRange = List(floor(pixelLoc.lon).toInt, ceil(pixelLoc.lon).toInt)

        val d = {
          for {
            xPos <- 0 to 1
            yPos <- 0 to 1
          } yield (xPos, yPos) -> grid(GridLocation(latRange(1 - yPos), lonRange(xPos)))
        }.toMap

        val xFraction = pixelLoc.lon - lonRange.head
        val yFraction = latRange(1) - pixelLoc.lat
        val temperature = bilinearInterpolation(CellPoint(xFraction, yFraction), d00 = d((0, 0)), d01 = d((0, 1)), d10 = d((1, 0)), d11 = d((1, 1)))
        val color: Color = interpolateColor(colors, temperature)
        Pixel(RGBColor(color.red, color.green, color.blue, 127))
      }
      .toArray

    Image(width, height, pixels)

  }

}
