package observatory

import observatory.Visualization.predictTemperature

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  private var gridLocToTemp = Map[GridLocation, Temperature]()

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {

    def calculateTempForGrid(gridLocation: GridLocation): Temperature = {
      val loc = Location(gridLocation.lat, gridLocation.lon)
      predictTemperature(temperatures, loc)
    }

    def memoizedCalculateTempForGrid(gridLocation: GridLocation): Temperature = {
      gridLocToTemp.get(gridLocation) match {
        case Some(t) => t
        case None =>
          val t = calculateTempForGrid(gridLocation)
          gridLocToTemp = gridLocToTemp + (gridLocation -> t)
          t
      }
    }

    memoizedCalculateTempForGrid
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    def averageInternal(gridLocation: GridLocation): Temperature = {
      val allTemp = temperaturess
        .map { locToTempSeq => makeGrid(locToTempSeq) }
        .map { fn => fn(gridLocation) }
      allTemp.sum / allTemp.size
    }

    averageInternal
  }

  /**
    * @param temperatures Known temperatures
    * @param normals      A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val gridFn = makeGrid(temperatures)
    //val avFn = average(Seq(temperatures))

    def deviationInternal(gridLocation: GridLocation): Temperature = {
      val temp = gridFn(gridLocation)
      val normalTemp = normals(gridLocation)
      temp - normalTemp
    }

    deviationInternal
  }


}

