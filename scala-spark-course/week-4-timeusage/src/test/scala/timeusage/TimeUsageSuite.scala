package timeusage

import org.apache.spark.sql.{Column, ColumnName, DataFrame, Row}
import org.apache.spark.sql.types.{DoubleType, StringType, StructField, StructType}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.util.Random

@RunWith(classOf[JUnitRunner])
class TimeUsageSuite extends FunSuite with BeforeAndAfterAll {

  test("classify should work") {
    assert(TimeUsage.classifiedColumns("t01" :: Nil) === (new Column("t01") :: Nil, Nil, Nil))
    assert(TimeUsage.classifiedColumns("t01" :: "t05" :: "t1805" :: Nil) ===
      (new ColumnName("t01") :: Nil, new Column("t1805") :: new Column("t05") :: Nil, Nil))
    assert(TimeUsage.classifiedColumns("t01" :: Nil) === (new Column("t01") :: Nil, Nil, Nil))
  }
}
