import mill._, scalalib._, scalafmt._

object app extends ScalaModule with ScalafmtModule {
  def scalaVersion = "3.1.0"

  def ivyDeps = Agg(ivy"org.typelevel::cats-core:2.7.0")

  object test extends Tests with TestModule.Utest with ScalafmtModule {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.10")
  }
}
