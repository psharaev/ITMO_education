plugins {
    kotlin("jvm") version "1.7.10"
    java
    application
}

group = "ru.ifmo.mpp"

repositories {
    mavenCentral()
}

java {
    toolchain {
        languageVersion.set(JavaLanguageVersion.of(11))
    }
}

dependencies {
    implementation(kotlin("stdlib-jdk8"))
}

sourceSets.main {
    java.srcDir("src")
}

sourceSets.test {
    java.srcDir("test")
}

application {
    mainClass.set("VerifyMonotonicClockKt")
}

tasks["build"].dependsOn("run")