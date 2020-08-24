

val sparkVersion = "2.3.0"

name := "xandr-export"

scalaVersion := "2.11.8"

version := "1.0"

libraryDependencies += "org.apache.spark" %% "spark-sql" % sparkVersion % "provided"

// libraryDependencies += "org.apache.spark" %% "spark-ml" % sparkVersion % "provided"
