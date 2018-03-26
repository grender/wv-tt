lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.grender.wv_tt",
      scalaVersion := "2.12.4"
    )),
    name := "wv-tt"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
