resolvers += Resolver.url(
  "typesafe sbt-plugins",
  url("https://dl.bintray.com/typesafe/sbt-plugins")
)(Resolver.ivyStylePatterns)

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "1.0.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.0.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "0.6.33")
addSbtPlugin("org.scala-native"   % "sbt-scala-native"              % "0.3.9")
addSbtPlugin("org.scoverage"      % "sbt-scoverage"                 % "1.6.1")
addSbtPlugin("io.crashbox"        % "sbt-gpg"                       % "0.2.1")
addSbtPlugin("com.github.gseitz"  % "sbt-release"                   % "1.0.13")
addSbtPlugin("com.47deg"          % "sbt-microsites"                % "1.2.0")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"               % "0.7.0")
