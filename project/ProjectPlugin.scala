import sbt.Keys._
import sbt._
import org.scalajs.sbtplugin.cross.CrossProject
import org.scalajs.sbtplugin.cross.CrossType
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {

    def module(
      modName: String
    ): CrossProject =
      CrossProject(
        modName,
        file(s"modules/$modName"),
        CrossType.Pure
      )
        .settings(moduleName := s"${name.value}-$modName")

    def athemaModule(
      modName: String
    ): CrossProject =
      CrossProject(
        s"athema-$modName",
        file(s"athema/$modName"),
        CrossType.Pure
      )
        .settings(moduleName := s"athema-$modName")

    lazy val noPublishSettings: Seq[Def.Setting[_]] = Seq(
      publish := ((): Unit),
      publishLocal := ((): Unit),
      publishArtifact := false)

    lazy val macroSettings: Seq[Setting[_]] = Seq(
      libraryDependencies ++= Seq(
        scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided,
        scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
        compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)))

  }

  override def projectSettings: Seq[Def.Setting[_]] = Seq(

    name := "droste",
    startYear := Option(2017),

    fork in Test := !isScalaJSProject.value,
    parallelExecution in Test := false,
    outputStrategy := Some(StdoutOutput),
    connectInput in run := true,
    cancelable in Global := true,

    crossScalaVersions :=  List("2.11.11", "2.12.3"),
    scalaVersion       := "2.12.3"
  )

}
