// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.tortoise

import org.nlogo.{ api, headless, nvm },
  nvm.FrontEndInterface.{ ProceduresMap, NoProcedures },
  headless.lang, lang._,
  org.nlogo.util.Femto,
  org.scalatest.Assertions

class TestReporters extends lang.TestReporters {
  override def shouldRun(t: LanguageTest, mode: TestMode) =
    mode == NormalMode && super.shouldRun(t, mode)
  override def withFixture[T](name: String)(body: AbstractFixture => T): T =
    body(new TortoiseFixture)
}

class TestCommands extends lang.TestCommands {
  override def shouldRun(t: LanguageTest, mode: TestMode) =
    mode == NormalMode && super.shouldRun(t, mode)
  override def withFixture[T](name: String)(body: AbstractFixture => T): T =
    body(new TortoiseFixture)
}

class TortoiseFixture extends AbstractFixture {

  import Assertions._

  val rhino = new Rhino

  def dimensions = api.WorldDimensions.square(0)

  var program: api.Program = api.Program.empty
  var procs: ProceduresMap = NoProcedures

  override def declare(source: String) {
    val (js, p, m) =
      try Compiler.compileProcedures(source, 0, 0, 0, 0)
      catch catcher
    program = p
    procs = m
    rhino.eval(js)
  }

  val compiler: nvm.CompilerInterface =
    Femto.scalaSingleton("org.nlogo.compile.Compiler")

  override def readFromString(literal: String): AnyRef =
    try rhino.eval(Compiler.compileReporter(literal))
    catch catcher

  override def open(path: String) = ???
  override def open(model: headless.ModelCreator.Model) {
    declare(model.code)
  }

  override def runCommand(command: Command, mode: TestMode) {
    if (!command.result.isInstanceOf[Success])
      cancel("unknown result type: " + command.result.getClass.getSimpleName)
    try rhino.run(Compiler.compileCommands(command.command, procs, program))
    catch catcher
  }

  override def runReporter(reporter: Reporter, mode: TestMode) {
    val actualResult =
      try rhino.eval(Compiler.compileReporter(reporter.reporter, procs, program))
      catch catcher
    reporter.result match {
      case Success(expectedResult) =>
        checkResult(mode, reporter.reporter, expectedResult, actualResult)
      case x =>
        cancel(s"not supported: $x")
    }
  }

  // kludginess ahead - ST 8/28/13

  val cancelers = Seq(
    "unknown primitive: ",
    "unknown settable: ",
    "unknown language feature: ")

  val catcher: PartialFunction[Throwable, Nothing] = {
    case ex: IllegalArgumentException
          if cancelers.exists(ex.getMessage.startsWith) =>
        cancel(ex.getMessage)
  }

}
