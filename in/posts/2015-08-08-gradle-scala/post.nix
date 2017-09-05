{ file-path, file-string, html, markdown, code }:

let
  h2 = html.h2 {};
  ol = html.ol {};
  li = html.li {};
  p = x: html.p {} (markdown x);

  bash   = code { language = "bash";   };
  groovy = code { language = "groovy"; };
  scala  = code { language = "scala";  };

in {
  title = "Gradle Scala REPL";
  date  = "2015 Aug 8";
  slug  = gradle-scala-repl;

  thumbnail = file-path ./thumbnail.png;

  abstract = ''
    I recently switched a Scala project’s build from SBT to Gradle
    and was disappointed to find that Gradle has no support for
    launching the Scala REPL. My workaround: …
  '';

  body = [

    (p ''
      I recently switched a Scala project’s build from SBT to Gradle and was
      disappointed to find that Gradle has no support for launching the
      Scala REPL.
    '')

    (p "My workaround:")

    (ol [
      (li "Let Gradle compile and output the classpath")
      (li "Launch the REPL separately")
    ])

    (p ''
      At first this felt like a kludge around a deficiency in Gradle, but now
      I think this approach may actually make more sense than SBT’s conflation
      of build tool and REPL. The build tool doesn’t need to be responsible for
      *everything*.
    '')

    ##################################################################

    (h2 "Launching the REPL from the scala-compiler jar")

    (p ''
      Normally you’d launch the REPL using the `scala` script provided by the
      standard Scala installation. But I’d prefer to let Gradle download the
      appropriate version of Scala for the project rather than requiring
      developers to install it themselves. Gradle can help us with this because
      the artifact `org.scala-lang:scala-compiler` in the Maven Central repo
      contains the Scala REPL.
    '')

    (p ''
      The `main` method that launches the REPL belongs to a class with the
      (rather non-obvious) name `scala.tools.nsc.MainGenericRunner`.
      Thus we need to run
    '')

    (bash ''
      java -Dscala.usejavacp=true \
           -classpath "$CLASSPATH" \
           scala.tools.nsc.MainGenericRunner
    '')

    (p "where `$CLASSPATH` includes the `scala-compiler` jar.")

    ##################################################################

    (h2 "Fetching the jar with Gradle")

    (p ''
      To let Gradle provide the scala-compiler artifact for us, add it
      as a `classpath` dependency to `buildscript`.
    '')

    (groovy ''
      buildscript {
          dependencies {
              classpath "org.scala-lang:scala-compiler:${scalaVersion}"
          }
          repositories {
              mavenCentral()
          }
      }
    '')

    (p ''
      Then add a function to look up the filesystem path of this artifact,
      which we’ll use later when assembling the full classpath for the
      REPL session.
    '')

    (groovy ''
      def scalaPath = buildscript.configurations.classpath.find {
          it.name == "scala-compiler-${scalaVersion}.jar"
      }
    '')

    ##################################################################

    (h2 "Generating the classpath")

    (p ''
      Getting the classpath for your project and its dependencies in Gradle
      is pretty simple.
    '')

    (groovy ''
      def classpath = sourceSets.main.runtimeClasspath.asPath
    '')

    (p ''
      Combined with the path of the Scala compiler, the result is the full
      classpath that we’ll use for launching the REPL.
    '')

    (groovy ''
      task printClasspath << {
          println "${scalaPath}:${classpath}"
      }
    '')

    (p ''
      With this task defined, call `gradle printClasspath --quiet` to set the
      classpath in the startup script.
    '')

    (bash ''
      java -Dscala.usejavacp=true \
           -classpath "$(gradle printClasspath --quiet)" \
           scala.tools.nsc.MainGenericRunner
    '')

    ##################################################################

    (h2 "Initial REPL commands")

    (p ''
      SBT has a useful setting that lets you specify Scala expressions that
      run automatically when the REPL starts. This tends to save you the
      trouble of repeating a lot of imports every time you start a session.
    '')

    (scala ''
      initialCommands in console := "..."
    '')

    (p ''
      You can accomplish this using the `-i` option on the Scala REPL, which
      loads commands from a file. In this example, the file containing initial
      commands is named `repl.scala`.
    '')

    (bash ''
      java -Dscala.usejavacp=true \
           -classpath "$(gradle printClasspath --quiet)" \
           scala.tools.nsc.MainGenericRunner \
           -i repl.scala
    '')

    ##################################################################

    (h2 "Full example")

    (p "**build.gradle**")

    (groovy (file-string ./build.gradle))

    (p "**repl.sh**")

    (bash ''
      #!/bin/bash
      gradle :repl:compileScala && \
      java -Dscala.usejavacp=true \
           -classpath "$(gradle :repl:printClasspath --quiet)" \
           scala.tools.nsc.MainGenericRunner \
           -i repl.scala
    '')

    (p "**repl.scala**")

    (scala ''
      myproject.repl.init()
      import myproject.repl._
    '')

  ];
}
