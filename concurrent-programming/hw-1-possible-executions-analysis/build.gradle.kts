import org.jetbrains.kotlin.gradle.plugin.*

plugins {
    kotlin("jvm") version "1.7.10"
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

application {
    mainClass.set("PossibleExecutionsVerifierKt")
}

tasks["build"].dependsOn("run")