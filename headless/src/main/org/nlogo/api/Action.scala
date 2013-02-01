// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.api

import scala.collection.mutable.{ Buffer, Publisher, Subscriber }

trait Action

trait ActionRunner[A <: Action] {
  def run(action: A): Unit
}

trait ActionBroker[A <: Action]
  extends Publisher[A] {
  val runner: ActionRunner[A]
  override def publish(action: A) {
    super.publish(action)
    runner.run(action)
  }
}

/**
 * An ActionBuffer logs all actions generated by the supplied
 * ActionBroker. Actions can be grabbed (which clears the buffer)
 * and the buffer can be cleared independently. NP 2013-01-25.
 */
class ActionBuffer[A <: Action](broker: ActionBroker[A])
  extends Subscriber[A, Publisher[A]] {
  broker.subscribe(this)
  private val buffer = Buffer[A]()
  override def notify(pub: Publisher[A], action: A) {
    buffer += action
  }
  def clear() { buffer.clear() }
  def grab(): List[A] = {
    val actions = buffer.toList
    clear()
    actions
  }
}
