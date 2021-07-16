//resolvers += Resolver.url(
//  "typesafe sbt-plugins",
//  url("https://dl.bintray.com/typesafe/sbt-plugins")
//)(Resolver.ivyStylePatterns)

addSbtPlugin("org.portable-scala"        % "sbt-scalajs-crossproject"      % "1.1.0")
addSbtPlugin("org.portable-scala"        % "sbt-scala-native-crossproject" % "1.1.0")
addSbtPlugin("org.scala-js"              % "sbt-scalajs"                   % "1.6.0")
addSbtPlugin("org.scala-native"          % "sbt-scala-native"              % "0.4.0")
addSbtPlugin("org.scoverage"             % "sbt-scoverage"                 % "1.8.2")
addSbtPlugin("io.crashbox"               % "sbt-gpg"                       % "0.2.1")
addSbtPlugin("com.github.gseitz"         % "sbt-release"                   % "1.0.13")
addSbtPlugin("com.geirsson"              % "sbt-ci-release"                % "1.5.7")
addSbtPlugin("com.47deg"                 % "sbt-microsites"                % "1.3.4")
addSbtPlugin("org.scalameta"             % "sbt-mdoc"                      % "2.2.21")
addSbtPlugin("com.alejandrohdezma"       % "sbt-github"                    % "0.9.3")
addSbtPlugin("com.alejandrohdezma"       % "sbt-github-mdoc"               % "0.9.3")
addSbtPlugin("com.typesafe"              % "sbt-mima-plugin"               % "0.7.0")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat"                  % "0.1.20")
