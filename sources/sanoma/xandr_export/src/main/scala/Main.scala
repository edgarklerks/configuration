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

    val datasetRaw = spark.sql(s"SELECT user_id_64, segments FROM nl_appnexus.auctionsegment_feed WHERE segments LIKE '%23302277%' AND day >= '$date'")
      val datasetAgg = datasetRaw
      .withColumn("parsed_segments",explode(from_json($"segments",segmentSchema)))
          .select($"user_id_64",concat_ws("#",$"parsed_segments.segment_id",lit("0")).as("parsed_segments"))
          .groupBy($"user_id_64")
          .agg(concat_ws(";",collect_list($"parsed_segments")))
          .select(concat_ws(":",$"user_id_64",$"parsed_segments"))

    datasetAgg.write.mode(SaveMode.Overwrite).csv("s3://sanoma-smbene-bigdata-user/e.klerks/xandr-test")
  }

}
