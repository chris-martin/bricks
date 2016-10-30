--------------------------------------------------------------------------------
title:     Gradle Scala REPL
date:      2015 Aug 8
thumbnail: thumbnail.png
slug:      gradle-scala-repl
abstract:  I recently switched a Scala project’s build from SBT to Gradle
           and was disappointed to find that Gradle has no support for
           launching the Scala REPL. My workaround: …
--------------------------------------------------------------------------------

I recently switched a Scala project’s build from SBT to Gradle and was
disappointed to find that Gradle has no support for launching the
Scala REPL.

My workaround:

1. Let Gradle compile and output the classpath
2. Launch the REPL separately

At first this felt like a kludge around a deficiency in Gradle, but now
I think this approach may actually make more sense than SBT’s conflation
of build tool and REPL. The build tool doesn’t need to be responsible for
*everything*.

Launching the REPL from the scala-compiler jar
--------------------------------------------------------------------------

Normally you’d launch the REPL using the `scala` script provided by the
standard Scala installation. But I’d prefer to let Gradle download the
appropriate version of Scala for the project rather than requiring
developers to install it themselves. Gradle can help us with this because
the artifact `org.scala-lang:scala-compiler` in the Maven Central repo
contains the Scala REPL.

The `main` method that launches the REPL belongs to a class with the
(rather non-obvious) name `scala.tools.nsc.MainGenericRunner`.
Thus we need to run

```bash
java -Dscala.usejavacp=true \
     -classpath "$CLASSPATH" \
     scala.tools.nsc.MainGenericRunner
```

where `$CLASSPATH` includes the `scala-compiler` jar.

Fetching the jar with Gradle
--------------------------------------------------------------------------

To let Gradle provide the scala-compiler artifact for us, add it
as a `classpath` dependency to `buildscript`.

```groovy
buildscript {
    dependencies {
        classpath "org.scala-lang:scala-compiler:${scalaVersion}"
    }
    repositories {
        mavenCentral()
    }
}
```

Then add a function to look up the filesystem path of this artifact,
which we’ll use later when assembling the full classpath for the
REPL session.

```groovy
def scalaPath = buildscript.configurations.classpath.find {
    it.name == "scala-compiler-${scalaVersion}.jar"
}
```

Generating the classpath
--------------------------------------------------------------------------

Getting the classpath for your project and its dependencies in Gradle
is pretty simple.

```groovy
def classpath = sourceSets.main.runtimeClasspath.asPath
```

Combined with the path of the Scala compiler, the result is the full
classpath that we’ll use for launching the REPL.

```groovy
task printClasspath << {
    println "${scalaPath}:${classpath}"
}
```

With this task defined, call `gradle printClasspath --quiet` to set the
classpath in the startup script.

```bash
java -Dscala.usejavacp=true \
     -classpath "$(gradle printClasspath --quiet)" \
     scala.tools.nsc.MainGenericRunner
```

Initial REPL commands
--------------------------------------------------------------------------

SBT has a useful setting that lets you specify Scala expressions that
run automatically when the REPL starts. This tends to save you the
trouble of repeating a lot of imports every time you start a session.

```scala
initialCommands in console := "..."
```

You can accomplish this using the `-i` option on the Scala REPL, which
loads commands from a file. In this example, the file containing initial
commands is named `repl.scala`.

```bash
java -Dscala.usejavacp=true \
     -classpath "$(gradle printClasspath --quiet)" \
     scala.tools.nsc.MainGenericRunner \
     -i repl.scala
```

Full example
--------------------------------------------------------------------------

**build.gradle**

```groovy
project(':repl') {

    def scalaVersion = '2.11.7'

    // Dependencies on any other projects that should be
    // accessible from the REPL context.
    dependencies {
        compile project(':example_project_1')
        compile project(':example_project_2')
    }

    // Require the scala-compiler jar
    buildscript {
        dependencies {
            classpath "org.scala-lang:scala-compiler:${scalaVersion}"
        }
        repositories {
            mavenCentral()
        }
    }

    // The path of the scala-compiler jar
    def scalaPath = buildscript.configurations.classpath.find {
        it.name == "scala-compiler-${scalaVersion}.jar"
    }

    // The classpath of this project and its dependencies
    def classpath = sourceSets.main.runtimeClasspath.asPath

    // Prints the classpath needed to launch the REPL
    task printClasspath << {
        println "${scalaPath}:${classpath}"
    }

}
```

**repl.sh**

```bash
#!/bin/bash
gradle :repl:compileScala && \
java -Dscala.usejavacp=true \
     -classpath "$(gradle :repl:printClasspath --quiet)" \
     scala.tools.nsc.MainGenericRunner \
     -i repl.scala
```

**repl.scala**

```scala
myproject.repl.init()
import myproject.repl._
```
