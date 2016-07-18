  // scalaVersion := "2.10.2"
  scalaVersion := "2.11.5"
  // resolvers ++= Seq(
  //  "scct-github-repository" at "http://mtkopone.github.com/scct/maven-repo"
  // )

  libraryDependencies += "org.encog" % "encog-core" % "3.3.0"
  //http://search.maven.org/remotecontent?filepath=org/encog/encog-core/3.3.0/encog-core-3.3.0.jar

  //  "edu.berkeley.cs" %% "chisel" % "latest.release"
  //
  //addCommandAlias("make", ";project dhdl;compile")
  //
    addCommandAlias("make", ";project dhdl; compile")
    addCommandAlias("pir", "; project apps; run-main")
