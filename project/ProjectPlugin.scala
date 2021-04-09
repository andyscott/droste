import com.typesafe.sbt.site.jekyll.JekyllPlugin.autoImport._
import com.typesafe.sbt.site.SitePlugin.autoImport._
import microsites.MicrositeKeys._
import sbt.Keys._
import sbt._
import sbtcrossproject.CrossProject
import sbtcrossproject.CrossPlugin.autoImport._
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport._
import sbtrelease.ReleasePlugin.autoImport._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements
  override def requires: Plugins      = plugins.JvmPlugin

  object autoImport {

    def module(
        modName: String,
        prefix: String = "modules/"
    ): CrossProject =
      CrossProject(modName, file(s"$prefix$modName"))(
        JSPlatform,
        JVMPlatform /*, NativePlatform // soon */ )
        .crossType(CrossType.Pure)
        .withoutSuffixFor(JVMPlatform)
        .build()
        .jvmSettings(Test / fork := true)
        .settings(moduleName := s"droste-$modName")

    def jvmModule(
        modName: String,
        prefix: String = "modules/"
    ): Project =
      Project(modName, file(s"$prefix$modName"))
        .settings(
          Test / fork := true,
          moduleName := s"droste-$modName"
        )

    // lazy val macroSettings: Seq[Setting[_]] = Seq(
    //   libraryDependencies ++= Seq(
    //     scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
    //     scalaOrganization.value % "scala-reflect"  % scalaVersion.value % Provided,
    //     compilerPlugin(
    //       "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.patch)
    //   ))

    lazy val noPublishSettings: Seq[Def.Setting[_]] = Seq(
      publish := ((): Unit),
      publishLocal := ((): Unit),
      publishArtifact := false)

    lazy val micrositeSettings = Seq(
      micrositeName := "Droste",
      micrositeDescription := "A recursion library for Scala",
      micrositeDocumentationUrl := "/docs/",
      micrositeAuthor := "Andy Scott",
      micrositeGithubOwner := "andyscott",
      micrositeGithubRepo := "droste",
      micrositeGitterChannelUrl := "droste-recursion/Lobby",
      micrositeExternalLayoutsDirectory := (Compile / resourceDirectory).value / "microsite" / "layouts",
      micrositeExternalIncludesDirectory := (Compile / resourceDirectory).value / "microsite" / "includes",
      makeSite / includeFilter := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg" | "*.json" | "CNAME",
      Jekyll / includeFilter := (makeSite / includeFilter).value,
      micrositePushSiteWith := GitHub4s
    )

    lazy val noScala213Settings: Seq[Def.Setting[_]] = Seq(
      crossScalaVersions ~= (_.filterNot(_.startsWith("2.13")))
    )

    lazy val noScala3Settings: Seq[Def.Setting[_]] = Seq(
      crossScalaVersions ~= (_.filterNot(_.startsWith("3")))
    )

  }

  def scalaVersionSpecificFolders(srcName: String, srcBaseDir: java.io.File, scalaVersion: String) = {
    def extraDirs(suffix: String) =
      List(CrossType.Pure, CrossType.Full)
        .flatMap(_.sharedSrcDir(srcBaseDir, srcName).toList.map(f => file(f.getPath + suffix)))
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, y))     => extraDirs("-2.x")
      case Some((0 | 3, _)) => extraDirs("-3.x")
      case _                => Nil
    }
  }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      organization := "io.higherkindness",
      name := "droste",
      startYear := Option(2018),
      Test / parallelExecution := false,
      outputStrategy := Some(StdoutOutput),
      run / connectInput := true,
      Global / cancelable := true,
      crossScalaVersions := List("2.12.13", "2.13.5", "3.0.0-RC2"),
      scalaVersion := "3.0.0-RC2",
      Compile / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("main", baseDirectory.value, scalaVersion.value),
      Test / unmanagedSourceDirectories ++= scalaVersionSpecificFolders("test", baseDirectory.value, scalaVersion.value),
    ) ++ publishSettings

  lazy val publishSettings = Seq(
    releaseCrossBuild := true,
    homepage := Some(url("https://github.com/andyscott/droste")),
    licenses := Seq(
      "Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    publishMavenStyle := true,
    Test /publishArtifact := false,
    pomIncludeRepository := (_ => false),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    autoAPIMappings := true,
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/andyscott/droste"),
        "scm:git:git@github.com:andyscott/droste.git"
      )
    ),
    developers := List(
      Developer(
        "andyscott",
        "Andy Scott",
        "andy.g.scott@gmail.com",
        url("https://twitter.com/andygscott")
      )
    )
  )

}
