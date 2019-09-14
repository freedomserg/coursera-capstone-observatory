package observatory

import java.time.LocalDate

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Visualization.visualize

object Main extends App {

//  println("Extracting data ... ")
//  val temp2015: Iterable[(LocalDate, Location, Temperature)] = locateTemperatures(2015, "/stations.csv", "/2015.csv")
//
//  println("Calculating average temperature ... ")
//  val temp2015average: Iterable[(Location, Temperature)] = locationYearlyAverageRecords(temp2015)
//
//  println("Building an image ... ")
//  val image = visualize(temp2015average, Seq(
//    (60, Color(255, 255, 255)),
//    (32, Color(255, 0, 0)),
//    (12, Color(255, 255, 0)),
//    (0, Color(0, 255, 255)),
//    (-15, Color(0, 0, 255)),
//    (-27, Color(255, 0, 255)),
//    (-50, Color(33, 0, 107)),
//    (-60, Color(0, 0, 0))
//  ))
//
//  image.output(new java.io.File("target/2015.png"))

}
