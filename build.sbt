
resolvers += Resolver.jcenterRepo
resolvers += Resolver.bintrayRepo("neelsmith", "maven")

scalaVersion := "2.12.4"
libraryDependencies ++= Seq(

  "edu.holycross.shot.cite" %% "xcite" % "4.1.0"
)
