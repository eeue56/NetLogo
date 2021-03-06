// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.prim.etc

import org.nlogo.agent.{ Agent, Link, Observer, Turtle }
import org.nlogo.api.{ I18N, Syntax }
import org.nlogo.nvm.{ Command, Context, EngineException }

class _face extends Command {
  override def syntax =
    Syntax.commandSyntax(
      Array(Syntax.TurtleType | Syntax.PatchType),
      "OT--", true)
  override def perform(context: Context) {
    perform_1(context, argEvalAgent(context, 0))
  }
  def perform_1(context: Context, target: Agent) {
    if(target.isInstanceOf[Link])
      throw new EngineException(context, this,
        I18N.errors.get("org.nlogo.prim.etc.$common.expectedTurtleOrPatchButGotLink"))
    if (target.id == -1)
      throw new EngineException(context, this,
        I18N.errors.getN("org.nlogo.$common.thatAgentIsDead", target.classDisplayName))
    context.agent match {
      case turtle: Turtle =>
        turtle.face(target, true)
      case observer: Observer =>
        observer.face(target)
    }
    context.ip = next
  }
}
