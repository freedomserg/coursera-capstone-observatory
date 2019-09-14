package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql.{Column, Row, SparkSession}
import org.apache.spark.sql.types._
import org.apache.log4j.{Level, Logger}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.functions._

import scala.util.Try


/**
  * 1st milestone: data extraction
  */
object Extraction {

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {

    val stationsStringRdd = spark.sparkContext.textFile(fsPath(stationsFile))

    val stationsRowRdd =
      stationsStringRdd
        .map(_.split(",").to[List])
        .filter(d => d.size == 4 && !d.drop(2).contains(null))
        .map(stationRow)

    val stationsDF =
      spark.createDataFrame(stationsRowRdd, stationDfSchema())


    val temperaturesStringRdd = spark.sparkContext.textFile(fsPath(temperaturesFile))
    val temperatureRowRdd =
      temperaturesStringRdd
        .map(_.split(",").to[List])
        .filter(d => d.size == 5 && !d.drop(2).contains(null))
        .map(temperatureRow)

    val temperaturesDF =
      spark.createDataFrame(temperatureRowRdd, temperatureDfSchema())


    val result = stationsDF
      .join(temperaturesDF,
      stationsDF("STN identifier") <=> temperaturesDF("STN identifier") &&
        stationsDF("WBAN identifier") <=> temperaturesDF("WBAN identifier"), "left_outer")
      .drop(temperaturesDF.col("STN identifier"))
      .drop(temperaturesDF.col("WBAN identifier"))

      val tempCproj = round((result.col("Temperature") - 32) * 5 / 9, 2).as("TemperatureCelsius")


    result.select(result("STN identifier"), result("WBAN identifier"), result("Month"), result("Day"), result("Latitude"), result("Longitude"), tempCproj)
      .collect()
      .map { r =>
        (
          LocalDate.of(year, r.getAs[String]("Month").toInt, r.getAs[String]("Day").toInt),
          Location(r.getAs[Double]("Latitude"), r.getAs[Double]("Longitude")),
          r.getAs[Temperature]("TemperatureCelsius")
        )
      }
  }

  /** @return The filesystem path of the given resource */
  private def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString

  /** @return An RDD Row compatible with the schema produced by `stationDfSchema`
    * @param line Raw fields
    */
  private def stationRow(line: List[String]): Row = {
    val stn = Try(line.head.toLong).getOrElse(null)
    val wban = Try(line.tail.head.toLong).getOrElse(null)
    val lat = line.drop(2).head.toDouble
    val long = line.drop(3).head.toDouble
    Row.fromSeq(Seq(stn, wban, lat, long))
  }

  private def stationDfSchema(): StructType = {
    val stn = StructField("STN identifier", LongType, nullable = true)
    val wban = StructField("WBAN identifier", LongType, nullable = true)
    val lat = StructField("Latitude", DoubleType, nullable = false)
    val long = StructField("Longitude", DoubleType, nullable = false)

    new StructType(Array(stn, wban, lat, long))
  }

  /** @return An RDD Row compatible with the schema produced by `temperatureDfSchema`
    * @param line Raw fields
    */
  private def temperatureRow(line: List[String]): Row = {
    val stn = Try(line.head.toLong).getOrElse(null)
    val wban = Try(line.tail.head.toLong).getOrElse(null)
    val m = line.drop(2).head
    val d = line.drop(3).head
    val t = line.drop(4).head.toDouble
    Row.fromSeq(Seq(stn, wban, m, d, t))
  }

  private def temperatureDfSchema(): StructType = {
    val stn = StructField("STN identifier", LongType, nullable = true)
    val wban = StructField("WBAN identifier", LongType, nullable = true)
    val m = StructField("Month", StringType, nullable = false)
    val d = StructField("Day", StringType, nullable = false)
    val t = StructField("Temperature", DoubleType, nullable = false)

    new StructType(Array(stn, wban, m, d, t))
  }


  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    averageRecordsInternal(spark.sparkContext.parallelize(records.toSeq)).collect()
  }

  def averageRecordsInternal(
                           records: RDD[(LocalDate, Location, Temperature)]
                         ): RDD[(Location, Temperature)] = {
    records
      .map(r => (r._2, (r._3, 1)))
        .reduceByKey { (r1, r2) => (r1._1 + r2._1, r1._2 + r2._2)}
        .mapValues(r => BigDecimal(r._1 / r._2).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble)
  }

}
