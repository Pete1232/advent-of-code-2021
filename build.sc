import mill._, scalalib._

object app extends ScalaModule {
  def scalaVersion = "3.1.0"

  object test extends Tests with TestModule.Utest {
    def ivyDeps = Agg(ivy"com.lihaoyi::utest::0.7.10")
  }
}
