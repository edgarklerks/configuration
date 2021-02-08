import org.apache.spark.sql.{SaveMode, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions._

object Main {

  def main(xs : Array[String]) : Unit = {
    val spark = SparkSession.builder()
      .appName("xarndr-export")
      .getOrCreate()
    import spark.sqlContext.implicits._

    val date = xs(0)

    val segmentSchema = ArrayType(
     StructType(
       Seq(
         StructField("segment_id",IntegerType,false),
         StructField("segment_value",IntegerType,true),
         StructField("last_seen_min",IntegerType,true)
       )
     )
    )

    val datasetRaw = spark.sql(s"SELECT user_id_64 FROM nl_appnexus.auctionsegment_feed WHERE segments LIKE '%23302277%' AND day = '$date' group by user_id_64")
      val datasetAgg = datasetRaw
          .select($"user_id_64",lit("22835726#0").as("segments"))
          .select(concat_ws(":",$"user_id_64",$"segments").as("csv"))
          .repartition(10)

    datasetAgg.write.mode(SaveMode.Overwrite).csv("s3://sanoma-smbene-bigdata-user/e.klerks/xandr-export")
  }

}
