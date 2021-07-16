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

  object ScalaV {
    val v212 = "2.12.14"
    val v213 = "2.13.6"
    val v3   = "3.0.0"
  }

  object autoImport {

    def module(
        modName: String,
        prefix: String = "modules/"
    ): CrossProject =
      CrossProject(modName, file(s"$prefix$modName"))(
        JSPlatform,
        JVMPlatform /*, NativePlatform // soon */
      ).crossType(CrossType.Pure)
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

    lazy val noPublishSettings: Seq[Def.Setting[_]] = Seq(
      publish := ((): Unit),
      publishLocal := ((): Unit),
      publishArtifact := false
    )

    lazy val micrositeSettings = Seq(
      micrositeName := "Droste",
      micrositeDescription := "A recursion library for Scala",
      micrositeDocumentationUrl := "/docs/",
      micrositeAuthor := "Andy Scott",
      micrositeGithubOwner := "higherkindness",
      micrositeGithubRepo := "droste",
      micrositeGitterChannelUrl := "droste-recursion/Lobby",
      micrositeExternalLayoutsDirectory := (Compile / resourceDirectory).value / "microsite" / "layouts",
      micrositeExternalIncludesDirectory := (Compile / resourceDirectory).value / "microsite" / "includes",
      makeSite / includeFilter := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.yml" | "*.md" | "*.svg" | "*.json" | "CNAME",
      Jekyll / includeFilter := (makeSite / includeFilter).value,
      micrositePushSiteWith := GitHub4s
    )

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
      crossScalaVersions := List(ScalaV.v212, ScalaV.v213, ScalaV.v3),
      scalaVersion := ScalaV.v3,
      libraryDependencies ++= on(2)(
        compilerPlugin(
          "org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full
        )
      ).value,
      // Add some more source directories
      Compile / unmanagedSourceDirectories ++= {
        val sourceDir = (Compile / sourceDirectory).value
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((3, _))  => Seq(sourceDir / "scala-2.13+")
          case Some((2, 12)) => Seq()
          case Some((2, _))  => Seq(sourceDir / "scala-2.13+")
          case _             => Seq()
        }
      }
    ) ++ publishSettings

  lazy val publishSettings = Seq(
    releaseCrossBuild := true,
    homepage := Some(url("https://github.com/higherkindness/droste")),
    licenses := Seq(
      "Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    pomIncludeRepository := (_ => false),
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    autoAPIMappings := true,
    developers := List(
      Developer(
        "andyscott",
        "Andy Scott",
        "andy.g.scott@gmail.com",
        url("https://twitter.com/andygscott")
      )
    )
  )

  def on[A](major: Int, minor: Int)(a: A): Def.Initialize[Seq[A]] =
    Def.setting {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some(v) if v == (major, minor) => Seq(a)
        case _                              => Nil
      }
    }

  def on[A](major: Int)(a: A): Def.Initialize[Seq[A]] =
    Def.setting {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((`major`, _)) => Seq(a)
        case _                  => Nil
      }
    }

  def onVersion[A](major: Int)(a: String => A): Def.Initialize[Seq[A]] =
    Def.setting {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((`major`, _)) => Seq(a(scalaVersion.value))
        case _                  => Nil
      }
    }
}
