// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.tortoise

import org.nlogo.api
import org.nlogo.headless, headless.lang, lang._
import org.scalatest.Assertions

class TestReporters extends lang.TestReporters {
  override def shouldRun(t: LanguageTest, mode: TestMode) =
    mode == NormalMode && super.shouldRun(t, mode)
  override def withFixture[T](name: String)(body: AbstractFixture => T): T =
    body(new TortoiseFixture)
}

class TortoiseFixture extends AbstractFixture {

  import Assertions._

  val rhino = new Rhino

  def dimensions = api.WorldDimensions.square(0)

  override def declare(source: String) {
    val (js, _, _) = Compiler.compileProcedures(source, 0, 0, 0, 0)
    rhino.eval(js)
  }

  val compiler: org.nlogo.nvm.CompilerInterface =
    org.nlogo.util.Femto.scalaSingleton("org.nlogo.compile.Compiler")

  override def readFromString(literal: String): AnyRef =
    rhino.eval(Compiler.compileReporter(literal))

  override def open(path: String) = ???
  override def open(model: headless.ModelCreator.Model) { }
  override def runCommand(command: Command, mode: TestMode) = ???
  override def runReporter(reporter: Reporter, mode: TestMode) {
    val actualResult =
      try rhino.eval(Compiler.compileReporter(reporter.reporter))
      catch {
        case ex: IllegalArgumentException
            if ex.getMessage.startsWith("unknown primitive: ") =>
          cancel(ex.getMessage)
      }
    reporter.result match {
      case Success(expectedResult) =>
        checkResult(mode, reporter.reporter, expectedResult, actualResult)
      case x =>
        cancel(s"not supported: $x")
    }
  }

}
