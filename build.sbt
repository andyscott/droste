import ProjectPlugin.ScalaV
import ProjectPlugin.on
import ProjectPlugin.onVersion

lazy val root = (project in file("."))
  .settings(noPublishSettings)
  .aggregate(coreJVM, coreJS)
  .aggregate(metaJVM, metaJS)
  .aggregate(macrosJVM, macrosJS)
  .aggregate(reftree)
  .aggregate(scalacheckJVM, scalacheckJS)
  .aggregate(lawsJVM, lawsJS)
  .aggregate(testsJVM, testsJS)
  .aggregate(athemaJVM, athemaJS)
  .aggregate(readme)
  .settings(
    crossScalaVersions := List()
  ) // work around https://github.com/sbt/sbt/issues/4181

lazy val publish = (project in file(".publish"))
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)
  .aggregate(coreJVM, coreJS)
  .aggregate(metaJVM, metaJS)
  .aggregate(macrosJVM, macrosJS)
  .aggregate(scalacheckJVM, scalacheckJS)
  .aggregate(lawsJVM, lawsJS)
  .aggregate(testsJVM, testsJS)

lazy val coverage = (project in file(".coverage"))
  .settings(
    noPublishSettings,
    crossScalaVersions := Seq(ScalaV.v212, ScalaV.v213),
    scalaVersion := ScalaV.v213
  )
  .settings(coverageEnabled := true)
  .aggregate(coreJVM)
  .aggregate(metaJVM)
  .aggregate(macrosJVM)
  .aggregate(scalacheckJVM)
  .aggregate(lawsJVM)
  .aggregate(testsJVM)

lazy val V = new {
  val cats             = "2.6.1"
  val collectionCompat = "2.5.0"
  val refined          = "0.9.26"
  val algebra          = "2.2.3"
  val atto             = "0.9.5"
  val scalacheck       = "1.15.4"
  val drostePrev       = "0.7.0"
}

lazy val meta = module("meta")
  .settings(
    mimaPreviousArtifacts := Set(
      organization.value %%% moduleName.value % V.drostePrev
    ),
    libraryDependencies ++= onVersion(2)(version =>
      Seq(
        "org.scala-lang" % "scala-reflect"  % version,
        "org.scala-lang" % "scala-compiler" % version % "provided"
      )
    ).value.flatten
  )

lazy val metaJVM = meta.jvm
lazy val metaJS  = meta.js

lazy val core = module("core")
  .dependsOn(meta)
  .settings(
    mimaPreviousArtifacts := Set(
      organization.value %%% moduleName.value % V.drostePrev
    ),
    mimaBinaryIssueFilters ++= {
      import com.typesafe.tools.mima.core.IncompatibleSignatureProblem
      import com.typesafe.tools.mima.core.ProblemFilters.exclude
      // See https://github.com/lightbend/mima/issues/423
      Seq(
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.Basis#Default.unapply"
        ),
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.GAlgebra#Gathered.unapply"
        ),
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.GAlgebraArrow.algebra"
        ),
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.GAlgebraM#Gathered.unapply"
        ),
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.GCoalgebra#Scattered.unapply"
        ),
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.GCoalgebraArrow.algebra"
        ),
        exclude[IncompatibleSignatureProblem](
          "higherkindness.droste.GCoalgebraM#Scattered.unapply"
        )
      )
    },
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % V.cats,
      "org.typelevel" %%% "cats-free" % V.cats
    )
  )

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js

lazy val macros = module("macros")
  .dependsOn(core)
  .settings(
    mimaPreviousArtifacts := Set(
      organization.value %%% moduleName.value % V.drostePrev
    ),
    libraryDependencies ++= on(2, 12)(
      compilerPlugin(
        "org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.patch
      )
    ).value,
    scalacOptions ++= on(2, 13)("-Ymacro-annotations").value
  )

lazy val macrosJVM = macros.jvm
lazy val macrosJS  = macros.js

lazy val reftree = jvmModule("reftree")
  .dependsOn(coreJVM)
  .settings(
    mimaPreviousArtifacts := Set(
      organization.value %% moduleName.value % V.drostePrev
    ),
    libraryDependencies ++= on(2, 12)(
      "io.github.stanch" %% "reftree" % "1.4.0"
    ).value
  )

lazy val scalacheck = module("scalacheck")
  .dependsOn(core)
  .settings(
    mimaPreviousArtifacts := Set(
      organization.value %%% moduleName.value % V.drostePrev
    ),
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % V.scalacheck
    )
  )

lazy val scalacheckJVM = scalacheck.jvm
lazy val scalacheckJS  = scalacheck.js

lazy val laws = module("laws")
  .dependsOn(core)
  .settings(
    mimaPreviousArtifacts := Set(
      organization.value %%% moduleName.value % V.drostePrev
    ),
    libraryDependencies ++= Seq(
      "org.scalacheck" %%% "scalacheck" % V.scalacheck
    )
  )

lazy val lawsJVM = laws.jvm
lazy val lawsJS  = laws.js

lazy val tests = module("tests")
  .dependsOn(core, scalacheck, laws, macros)
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalacheck"         %%% "scalacheck"              % V.scalacheck,
      "org.typelevel"          %%% "cats-laws"               % V.cats,
      "eu.timepit"             %%% "refined"                 % V.refined,
      "eu.timepit"             %%% "refined-scalacheck"      % V.refined,
      "org.scala-lang.modules" %%% "scala-collection-compat" % V.collectionCompat % Test
    ),
    libraryDependencies ++= on(2, 12)(
      compilerPlugin(
        "org.scalamacros" %% "paradise" % "2.1.1" cross CrossVersion.patch
      )
    ).value,
    scalacOptions ++= on(2, 13)("-Ymacro-annotations").value,
    // 'nowarn' doesn't work on scala3 yet, make warnings not fatal.
    scalacOptions --= on(3)("-Xfatal-warnings").value
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js

lazy val athema = module("athema", prefix = "")
  .dependsOn(core)
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)
  .settings(
    libraryDependencies ++=
      Seq(
        "org.typelevel" %%% "algebra"   % V.algebra,
        "org.tpolecat"  %%% "atto-core" % V.atto
      ) ++
        Seq(
          "org.scalacheck" %%% "scalacheck" % V.scalacheck
        ).map(_ % "test"),
    // 'nowarn' doesn't work on scala3 yet, make warnings not fatal.
    scalacOptions --= on(3)("-Xfatal-warnings").value
  )

lazy val athemaJVM = athema.jvm
lazy val athemaJS  = athema.js

lazy val readme = (project in file("modules/readme"))
  .enablePlugins(MdocPlugin)
  .dependsOn(coreJVM)
  .dependsOn(athemaJVM)
  .settings(noPublishSettings)
  .disablePlugins(MimaPlugin)
  .settings(mdocIn := file("modules/readme/docs"))
  .settings(mdocOut := (LocalRootProject / baseDirectory).value)

///////////////
//// DOCS ////
///////////////

lazy val docs = (project in file("docs"))
//  .dependsOn(coreJVM)
//  .dependsOn(athemaJVM)
  .settings(moduleName := "droste-docs")
  .settings(micrositeSettings: _*)
  .settings(noPublishSettings: _*)
  .enablePlugins(MicrositesPlugin)
  .disablePlugins(ProjectPlugin)
  .disablePlugins(MimaPlugin)

//////////////////
//// ALIASES /////
//////////////////

addCommandAlias(
  "ci-test",
  ";+clean;+test"
)
addCommandAlias("ci-docs", ";github;mdoc")

ThisBuild / resolvers += Resolver.sonatypeRepo("public")
