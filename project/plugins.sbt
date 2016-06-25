addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("org.scala-lang.modules.scalajs" %% "scalajs-sbt-plugin" % "0.5.6")

addSbtPlugin("com.kalmanb.sbt" % "sbt-ctags" % "0.3.0")

resolvers += Resolver.url("scala-js-snapshots", url("http://repo.scala-js.org/repo/releases/"))(Resolver.ivyStylePatterns)

