val projectName = "guardrail-root"
name := projectName
organization in ThisBuild := "com.twilio"
licenses in ThisBuild += ("MIT", url("http://opensource.org/licenses/MIT"))
// Project version is determined by sbt-git based on the most recent tag
// Publishing information is defined in `bintray.sbt`

enablePlugins(GitVersioning)
git.useGitDescribe := true

crossScalaVersions in ThisBuild := Seq("2.12.8")

val akkaVersion       = "10.0.14"
val catsVersion       = "1.6.0"
val catsEffectVersion = "1.0.0"
val circeVersion      = "0.10.1"
val http4sVersion     = "0.20.0"
val scalatestVersion  = "3.0.8"
val javaparserVersion = "3.7.1"
val endpointsVersion  = "0.8.0"
val ahcVersion        = "2.8.1"
val dropwizardVersion = "1.3.9"
val jerseyVersion     = "2.25.1"

mainClass in assembly := Some("com.twilio.guardrail.CLI")
assemblyMergeStrategy in assembly := {
  case ".api_description" => MergeStrategy.discard
  case ".options" => MergeStrategy.concat
  case "plugin.properties" => MergeStrategy.discard
  case "plugin.xml" => MergeStrategy.concat
  case "META-INF/eclipse.inf" => MergeStrategy.first
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

// (filename, prefix, tracing)
def sampleResource(name: String): java.io.File = file(s"modules/sample/src/main/resources/${name}")
val exampleCases: List[(java.io.File, String, Boolean, List[String])] = List(
  (sampleResource("additional-properties.yaml"), "additionalProperties", false, List.empty),
  (sampleResource("alias.yaml"), "alias", false, List.empty),
  (sampleResource("contentType-textPlain.yaml"), "tests.contentTypes.textPlain", false, List.empty),
  (sampleResource("custom-header-type.yaml"), "tests.customTypes.customHeader", false, List.empty),
  (sampleResource("edgecases/defaults.yaml"), "edgecases.defaults", false, List.empty),
  (sampleResource("formData.yaml"), "form", false, List.empty),
  (sampleResource("issues/issue45.yaml"), "issues.issue45", false, List.empty),
  (sampleResource("issues/issue121.yaml"), "issues.issue121", false, List.empty),
  (sampleResource("issues/issue127.yaml"), "issues.issue127", false, List.empty),
  (sampleResource("issues/issue143.yaml"), "issues.issue143", false, List.empty),
  (sampleResource("issues/issue148.yaml"), "issues.issue148", false, List.empty),
  (sampleResource("issues/issue164.yaml"), "issues.issue164", false, List.empty),
  (sampleResource("issues/issue184.yaml"), "issues.issue184", false, List.empty),
  (sampleResource("issues/issue215.yaml"), "issues.issue215", false, List.empty),
  (sampleResource("issues/issue218.yaml"), "issues.issue218", false, List.empty),
  (sampleResource("issues/issue222.yaml"), "issues.issue222", false, List.empty),
  (sampleResource("issues/issue223.yaml"), "issues.issue223", false, List.empty),
  (sampleResource("issues/issue249.yaml"), "issues.issue249", false, List.empty),
  (sampleResource("issues/issue264.yaml"), "issues.issue264", false, List.empty),
  (sampleResource("issues/issue325.yaml"), "issues.issue325", false, List.empty),
  (sampleResource("issues/issueX.yaml"), "issues.issueX", false, List.empty),
  (sampleResource("multipart-form-data.yaml"), "multipartFormData", false, List.empty),
  (sampleResource("petstore.json"), "examples", false, List("--import", "support.PositiveLong")),
  (sampleResource("plain.json"), "tests.dtos", false, List.empty),
  (sampleResource("polymorphism.yaml"), "polymorphism", false, List.empty),
  (sampleResource("polymorphism-mapped.yaml"), "polymorphismMapped", false, List.empty),
  (sampleResource("raw-response.yaml"), "raw", false, List.empty),
  (sampleResource("redaction.yaml"), "redaction", false, List.empty),
  (sampleResource("server1.yaml"), "tracer", true, List.empty),
  (sampleResource("server2.yaml"), "tracer", true, List.empty),
  (sampleResource("pathological-parameters.yaml"), "pathological", false, List.empty)
)

val exampleFrameworkSuites = Map(
  "scala" -> List(
    ("akka-http", "akkaHttp", List("client", "server")),
    ("endpoints", "endpoints", List("client")),
    ("http4s", "http4s", List("client", "server"))
  ),
  "java" -> List(
    ("dropwizard", "dropwizard", List("client", "server"))
  )
)

def exampleArgs(language: String): List[List[String]] = exampleCases
  .foldLeft(List[List[String]](List(language)))({
    case (acc, (path, prefix, tracing, extra)) =>
      acc ++ (for {
        frameworkSuite <- exampleFrameworkSuites(language)
        (frameworkName, frameworkPackage, kinds) = frameworkSuite
        kind <- kinds
        tracingFlag = if (tracing && language != "java") Option("--tracing") else Option.empty[String]
      } yield
        (
          List(s"--${kind}") ++
            List("--specPath", path.toString()) ++
            List("--outputPath", s"modules/sample-${frameworkPackage}/target/generated") ++
            List("--packageName", s"${prefix}.${kind}.${frameworkPackage}") ++
            List("--framework", frameworkName)
        ) ++ tracingFlag ++ extra)
  })

lazy val runJavaExample: TaskKey[Unit] = taskKey[Unit]("Run scala generator with example args")
fullRunTask(
  runJavaExample,
  Test,
  "com.twilio.guardrail.CLI",
  exampleArgs("java").flatten.filter(_.nonEmpty): _*
)

lazy val runScalaExample: TaskKey[Unit] = taskKey[Unit]("Run scala generator with example args")
fullRunTask(
  runScalaExample,
  Test,
  "com.twilio.guardrail.CLI",
  exampleArgs("scala").flatten.filter(_.nonEmpty): _*
)

artifact in (Compile, assembly) := {
  (artifact in (Compile, assembly)).value
    .withClassifier(Option("assembly"))
}

addArtifact(artifact in (Compile, assembly), assembly)

val resetSample = TaskKey[Unit]("resetSample", "Reset sample module")
val scalaFrameworks = exampleFrameworkSuites("scala").map(_._2)
val javaFrameworks = exampleFrameworkSuites("java").map(_._2)

resetSample := {
  import scala.sys.process._
  (List("sample") ++ (scalaFrameworks ++ javaFrameworks).map(x => s"sample-${x}"))
    .foreach(sampleName => s"git clean -fdx modules/${sampleName}/target/generated" !)
}

// Deprecated command
addCommandAlias("example", "runtimeSuite")

addCommandAlias("cli", "runMain com.twilio.guardrail.CLI")
addCommandAlias("runtimeScalaSuite", "; resetSample ; runScalaExample ; " + scalaFrameworks.map(x => s"${x}Sample/test").mkString("; "))
addCommandAlias("runtimeJavaSuite", "; resetSample ; runJavaExample ; " + javaFrameworks.map(x => s"${x}Sample/test").mkString("; "))
addCommandAlias("runtimeSuite", "; runtimeScalaSuite ; runtimeJavaSuite")
addCommandAlias("scalaTestSuite", "; codegen/test ; runtimeScalaSuite")
addCommandAlias("javaTestSuite", "; codegen/test ; runtimeJavaSuite")
addCommandAlias("format", "; codegen/scalafmt ; codegen/test:scalafmt ; " + scalaFrameworks.map(x => s"${x}Sample/scalafmt ; ${x}Sample/test:scalafmt").mkString("; "))
addCommandAlias("checkFormatting", "; codegen/scalafmtCheck ; " + scalaFrameworks.map(x => s"${x}Sample/scalafmtCheck ; ${x}Sample/test:scalafmtCheck").mkString("; "))
addCommandAlias("testSuite", "; scalaTestSuite ; javaTestSuite")

addCommandAlias(
  "publishBintray",
  "; set publishTo in codegen := (publishTo in bintray in codegen).value; codegen/publishSigned"
)
addCommandAlias(
  "publishSonatype",
  "; set publishTo in codegen := (sonatypePublishTo in codegen).value; codegen/publishSigned"
)

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" % "kind-projector"  % "0.9.10" cross CrossVersion.binary)

publishMavenStyle := true

val testDependencies = Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test
)

val excludedWarts = Set(Wart.DefaultArguments, Wart.Product, Wart.Serializable, Wart.Any)
val codegenSettings = Seq(
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.10"),
  wartremoverWarnings in Compile ++= Warts.unsafe.filterNot(w => excludedWarts.exists(_.clazz == w.clazz)),
  wartremoverWarnings in Test := List.empty,
  scalacOptions in ThisBuild ++= Seq(
    "-Ypartial-unification",
    "-Ydelambdafy:method",
    // "-Ywarn-unused-import",  // TODO: Enable this! https://github.com/twilio/guardrail/pull/282
    "-feature",
    "-unchecked",
    "-deprecation",
    "-encoding",
    "utf8"
  ) ++ (if (scalaVersion.value.startsWith("2.11.")) {
          List("-Xexperimental", "-Xlint:_")
        } else {
          List("-Xlint:-unused,_")
        }),
  parallelExecution in Test := true
)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= testDependencies,
    skip in publish := true
  )
  .dependsOn(codegen)

lazy val codegen = (project in file("modules/codegen"))
  .settings(
    (name := "guardrail") +:
      codegenSettings,
    libraryDependencies ++= testDependencies ++ Seq(
      "org.scalameta"               %% "scalameta"                    % "4.1.0",
      "com.github.javaparser"       % "javaparser-symbol-solver-core" % javaparserVersion,
      "org.eclipse.jdt"             % "org.eclipse.jdt.core"          % "3.17.0",
      "org.eclipse.platform"        % "org.eclipse.equinox.app"       % "1.3.600",
      "io.swagger.parser.v3"        % "swagger-parser"                % "2.0.12",
      "org.tpolecat"                %% "atto-core"                    % "0.6.3",
      "org.typelevel"               %% "cats-core"                    % catsVersion,
      "org.typelevel"               %% "cats-kernel"                  % catsVersion,
      "org.typelevel"               %% "cats-macros"                  % catsVersion,
      "org.typelevel"               %% "cats-free"                    % catsVersion,
      "org.scala-lang.modules"      %% "scala-java8-compat"           % "0.9.0",
    ),
    scalacOptions ++= List(
      "-language:higherKinds",
      "-Ywarn-unused-import",
    ),
    bintrayRepository := {
      if (isSnapshot.value) "snapshots"
      else "releases"
    },
    bintrayPackageLabels := Seq(
      "codegen",
      "openapi",
      "swagger"
    ),
    description := "Principled code generation for Scala services from OpenAPI specifications",
    homepage := Some(url("https://github.com/twilio/guardrail")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/twilio/guardrail"),
        "scm:git@github.com:twilio/guardrail.git"
      )
    ),
    developers := List(
      Developer(
        id = "blast_hardcheese",
        name = "Devon Stewart",
        email = "blast@hardchee.se",
        url = url("http://hardchee.se/")
      )
    )
  )

lazy val akkaHttpSample = (project in file("modules/sample-akkaHttp"))
  .settings(
    codegenSettings,
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http"         % akkaVersion,
      "com.typesafe.akka" %% "akka-http-testkit" % akkaVersion,
      "io.circe"          %% "circe-core"        % circeVersion,
      "io.circe"          %% "circe-generic"     % circeVersion,
      "io.circe"          %% "circe-java8"       % circeVersion,
      "io.circe"          %% "circe-parser"      % circeVersion,
      "org.scalatest"     %% "scalatest"         % scalatestVersion % Test,
      "org.typelevel"     %% "cats-core"         % catsVersion
    ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated",
    skip in publish := true,
    scalafmtOnCompile := false
  )

lazy val http4sSample = (project in file("modules/sample-http4s"))
  .settings(
    codegenSettings,
    libraryDependencies ++= Seq(
      "io.circe"      %% "circe-core"          % circeVersion,
      "io.circe"      %% "circe-generic"       % circeVersion,
      "io.circe"      %% "circe-java8"         % circeVersion,
      "io.circe"      %% "circe-parser"        % circeVersion,
      "org.http4s"    %% "http4s-blaze-client" % http4sVersion,
      "org.http4s"    %% "http4s-blaze-server" % http4sVersion,
      "org.http4s"    %% "http4s-circe"        % http4sVersion,
      "org.http4s"    %% "http4s-dsl"          % http4sVersion,
      "org.scalatest" %% "scalatest"           % scalatestVersion % Test,
      "org.typelevel" %% "cats-core"           % catsVersion,
      "org.typelevel" %% "cats-effect"         % catsEffectVersion
    ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated",
    skip in publish := true,
    scalafmtOnCompile := false
  )

lazy val endpointsSample = (project in file("modules/sample-endpoints"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    codegenSettings,
    libraryDependencies ++= Seq(
      "io.circe"          %%% "circe-core"                    % circeVersion,
      "io.circe"          %%% "circe-generic"                 % circeVersion,
      "io.circe"          %%% "circe-java8"                   % circeVersion,
      "io.circe"          %%% "circe-parser"                  % circeVersion,
      "io.github.cquiroz" %%% "scala-java-time"               % "2.0.0-M13",
      "org.julienrf"      %%% "endpoints-algebra"             % endpointsVersion,
      "org.julienrf"      %%% "endpoints-json-schema-generic" % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client"          % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client-circe"    % endpointsVersion,
      "org.julienrf"      %%% "endpoints-xhr-client-faithful" % endpointsVersion,
      "org.scalatest"     %%% "scalatest"                     % scalatestVersion % Test,
      "org.typelevel"     %%% "cats-core"                     % catsVersion
    ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated",
    skip in publish := true,
    scalafmtOnCompile := false
  )

lazy val dropwizardSample = (project in file("modules/sample-dropwizard"))
  .settings(
    codegenSettings,
    javacOptions ++= Seq(
      "-Xlint:all"
    ),
    testOptions in Test += Tests.Argument(TestFrameworks.JUnit, "-a", "-v"),
    libraryDependencies ++= Seq(
      "io.dropwizard"              %  "dropwizard-core"        % dropwizardVersion,
      "io.dropwizard"              %  "dropwizard-forms"       % dropwizardVersion,
      "org.asynchttpclient"        %  "async-http-client"      % ahcVersion,
      "org.scala-lang.modules"     %% "scala-java8-compat"     % "0.9.0"            % Test,
      "org.scalatest"              %% "scalatest"              % scalatestVersion   % Test,
      "junit"                      %  "junit"                  % "4.12"             % Test,
      "com.novocode"               %  "junit-interface"        % "0.11"             % Test,
      "org.mockito"                %% "mockito-scala"          % "1.2.0"            % Test,
      "com.github.tomakehurst"     %  "wiremock"               % "1.57"             % Test,
      "io.dropwizard"              %  "dropwizard-testing"     % dropwizardVersion  % Test,
			"org.glassfish.jersey.test-framework.providers" % "jersey-test-framework-provider-grizzly2" % jerseyVersion % Test
    ),
    unmanagedSourceDirectories in Compile += baseDirectory.value / "target" / "generated",
    crossPaths := false,  // strangely needed to get the JUnit tests to run at all
    skip in publish := true,
    scalafmtOnCompile := false
  )

watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.scala").get
watchSources ++= (baseDirectory.value / "modules/sample/src/test" ** "*.java").get

logBuffered in Test := false

lazy val runExample: TaskKey[Unit] = taskKey[Unit]("Run with example args")

fullRunTask(
  runExample,
  Test,
  "com.twilio.guardrail.CLI",
  """
  --defaults
  --server --specPath modules/sample/src/main/resources/issues/issueX.yaml --outputPath modules/sample/src/main/scala --packageName issues.issueX
""".replaceAllLiterally("\n", " ").split(' ').filter(_.nonEmpty): _*
)