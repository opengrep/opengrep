plugins {
    kotlin("jvm") version "1.8.22"
    application
}

repositories {
    mavenCentral()
}

application {
    mainClass.set("constructor_taint.MainKt")
}
