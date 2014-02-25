name := "ForgePreprocessor"

version := "0.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

sbtPlugin := true
