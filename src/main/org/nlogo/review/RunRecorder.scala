// (C) Uri Wilensky. https://github.com/NetLogo/NetLogo

package org.nlogo.review

import scala.collection.mutable.Publisher

import org.nlogo.api
import org.nlogo.mirror.{ FixedViewSettings, Frame, Mirrorables, ModelRun }
import org.nlogo.swing.Implicits.thunk2runnable
import org.nlogo.window.{ GUIWorkspace, MonitorWidget, WidgetWrapperInterface }

import javax.swing.JOptionPane

sealed trait RunRecorderEvent
case class FrameAddedEvent(run: ModelRun, frame: Frame) extends RunRecorderEvent

class RunRecorder(
  ws: GUIWorkspace,
  tabState: ReviewTabState,
  saveModel: () => String,
  widgetHooks: () => Seq[WidgetHook],
  disableRecording: () => Unit
  ) extends Publisher[RunRecorderEvent] {
  override type Pub = Publisher[RunRecorderEvent]

  private val plotActionBuffer = new api.ActionBuffer(ws.plotManager)
  private val drawingActionBuffer = new api.ActionBuffer(ws.drawingActionBroker)
  private val actionBuffers = Vector(plotActionBuffer, drawingActionBuffer)

  ws.listenerManager.addListener(
    new api.NetLogoAdapter {
      override def requestedDisplayUpdate() {
        if (tabState.recordingEnabled) {
          if (!tabState.currentlyRecording) {
            ws.waitFor(() => startNewRun())
          }
          updateMonitors()
          // switch from job thread to event thread
          ws.waitFor(() => grab())
        }
      }
      override def afterModelOpened() {
        stopRecording()
        for {
          run <- tabState.currentRun
          if saveModel() != run.modelString
        } {
          // if we just opened a model different from the
          // one loaded from the previously current run...
          tabState.currentRun = None
        }
      }
      override def tickCounterChanged(ticks: Double) {
        if (ticks == -1.0) stopRecording()
      }
    })

  def grab() {
    for (run <- tabState.currentRun) {
      try {
        val widgetValues = widgetHooks()
          .map(_.valueStringGetter.apply)
          .zipWithIndex
        val mirrorables = Mirrorables.allMirrorables(ws.world, widgetValues)
        val actions = actionBuffers.flatMap(_.grab())
        val newFrame = run.appendData(mirrorables, actions)
        publish(FrameAddedEvent(run, newFrame))
      } catch {
        case e: java.lang.OutOfMemoryError =>
          JOptionPane.showMessageDialog(null,
            "Not enough memory. Turning off recording.",
            "Low memory", JOptionPane.WARNING_MESSAGE)
          disableRecording()
      }
    }
  }

  def stopRecording() {
    for (buffer <- actionBuffers) {
      buffer.clear()
      buffer.suspend()
    }
    for (run <- tabState.currentRun) {
      run.stillRecording = false
    }
  }

  def startNewRun() {

    val wrapper = org.nlogo.awt.Hierarchy.findAncestorOfClass(
      ws.viewWidget, classOf[WidgetWrapperInterface])
    val wrapperPos = wrapper.map(_.getLocation).getOrElse(new java.awt.Point(0, 0))

    // The position is the position of the view, but the image is the
    // whole interface, including the view.
    val view = ws.viewWidget.view
    val viewArea = new java.awt.geom.Area(new java.awt.Rectangle(
      wrapperPos.x + view.getLocation().x, wrapperPos.y + view.getLocation().y,
      view.getWidth, view.getHeight))

    // remove widgets from the clip area of the view:
    val container = ws.viewWidget.findWidgetContainer
    for {
      w <- widgetHooks().map(_.widget)
      bounds = container.getUnzoomedBounds(w)
      widgetArea = new java.awt.geom.Area(bounds)
    } viewArea.subtract(widgetArea)

    val viewSettings = FixedViewSettings(ws.view)

    val interfaceImage = org.nlogo.awt.Images.paintToImage(
      ws.viewWidget.findWidgetContainer.asInstanceOf[java.awt.Component])

    val name = Option(ws.getModelFileName).map(ReviewTab.removeExtension)
      .orElse(tabState.currentRun.map(_.name))
      .getOrElse("Untitled")

    val initialPlots = ws.plotManager.plots.map(_.clone)
    val initialDrawingImage = org.nlogo.drawing.cloneImage(ws.getAndCreateDrawing(false))
    val run = new ModelRun(
      name, saveModel(),
      viewArea, viewSettings, interfaceImage,
      initialPlots, initialDrawingImage,
      "", Nil)
    actionBuffers.foreach(_.activate())
    tabState.addRun(run)
  }

  def updateMonitors() {
    widgetHooks()
      .collect { case WidgetHook(m: MonitorWidget, _) => m }
      .foreach(updateMonitor)
  }

  // only callable from the job thread, and only once evaluator.withContext(...) has properly
  // set up ProcedureRunner's execution context. it would be problematic to try to trigger
  // monitors to update from the event thread, because the event thread is not allowed to
  // block waiting for the job thread. - ST 10/12/12
  def updateMonitor(monitor: MonitorWidget) {
    for {
      reporter <- monitor.reporter
      runner = ws.evaluator.ProcedureRunner
      if runner.hasContext
      result = try {
        runner.report(reporter)
      } catch {
        case _: api.LogoException => "N/A"
      }
    } monitor.value(result)
  }

}
