lazy val baseName         = "Unlike"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "A photo/video experiment",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.11.7",
  licenses            := Seq(cc_by_nc_nd),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"          %% "fileutil"           % "1.1.1",
    "de.sciss"          %% "numbers"            % "0.1.1",
    "de.sciss"          %% "processor"          % "0.4.0",
    "com.mortennobel"   %  "java-image-scaling" % "0.8.6",  // includes jh filters
    "de.sciss"          %% "audiowidgets-swing" % "1.9.1",
    "de.sciss"          %% "desktop"            % "0.7.1",
    "de.sciss"          %% "guiflitz"           % "0.5.0",
    "de.sciss"          %% "play-json-sealed"   % "0.2.0",
    "de.sciss"          %% "kollflitz"          % "0.2.0",
    "de.sciss"          %  "weblaf"             % "1.28",
    "com.github.scopt"  %% "scopt"              % "3.3.0"
  )
)

lazy val cc_by_nc_nd = "CC BY-NC-ND 4.0" -> url("http://creativecommons.org/licenses/by-nc-nd/4.0/legalcode")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)

// -------------

//mainClass in assembly := Some("de.sciss.unlike.Main")
//
//assemblyJarName in assembly := s"$baseName.jar"