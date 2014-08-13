package music

import com.typesafe.scalalogging.LazyLogging
import music.MusicActor._
import music.Patterns._


object MusicActor {

  /**
   * [PIA <: PatternItem[MusicActor], MusicActorPattern <% Pattern[MusicActor, PIA]]
   */
  type MusicActorPattern = Pattern[MusicActor, PatternItem[MusicActor]]
  type TimeItemBuilderPattern = Pattern[TimeItemBuilder, PatternItem[TimeItemBuilder]]

  val emptyActor: MusicActorPattern = constant(EmptyActor)

  def withActor(actor: MusicActor)(fn: MusicActor => MusicActor): MusicActor = {
    fn(actor)
    actor
  }
}

/**
 * Base trait for all actors.
 */
trait MusicActor extends LazyLogging {
  protected def receive: PartialFunction[MusicEvent, Unit]

  private def aroundReceive(event: MusicEvent) = receive.applyOrElse(event, unhandled)

  private def unhandled(event: MusicEvent): Unit = {
    event match {
      case _  => sys.error(s"$event is unhandled by ${this.getClass.getSimpleName}")
    }
  }

  def listen(actor: MusicActor): MusicActor
  def listen(l: MusicActorPattern): MusicActor

  def tell(event: MusicEvent) {
    logger.trace(s"$event -> $this")
    aroundReceive(event)
  }
}

trait NodeActor extends MusicActor {
  var listeners: MusicActorPattern

  def listen(l: MusicActorPattern) = {
    listeners = l
    this
  }

  def listen(actor: MusicActor) = {
    listeners = constant(actor)
    actor
  }
}

trait LeafActor extends MusicActor {
  override def listen(actor: MusicActor): MusicActor = sys.error(s"$this cant send a events to $actor because it is a LeafActor")
  override def listen(l: MusicActorPattern): MusicActor = sys.error(s"$this cant send a events to $l because it is a LeafActor")
}

object EmptyActor extends LeafActor {
  def receive = {
    case event: MusicEvent =>
  }
}

trait MusicEvent


object PrinterActor extends LeafActor {
  def receive = {
      case event: MusicEvent => logger.info(s"$event")
  }
  override def toString() = "PrinterActor"
}

case class TimeItemEvent(timeItem: TimeItem) extends MusicEvent

case class TimeItemBuilderActor(timeItemBuilders: TimeItemBuilderPattern, var listeners: MusicActorPattern = emptyActor) extends NodeActor {

  def receive = {
    case TimeItemEvent(timeItem) =>
      val timeItems = timeItemBuilders.takeItem().build(timeItem.start, timeItem.duration)
      listeners.takeItem().tell(TimeItemsEvent(timeItems))
  }
}

case class TimeItemsEvent(timeItems: List[TimeItem]) extends MusicEvent

case class TimeItemSplitterActor(var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  def receive = {
    case TimeItemsEvent(timeItems) =>
      timeItems.foreach {
        timeItem =>
        listeners.takeItem().tell(TimeItemEvent(timeItem))
    }
  }
}

case class TimeItemsTransformerActor(transformer: TimeItemTransformer, nrOfTransformations: Int = 1, includeOriginal: Boolean = false, var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  override def receive: PartialFunction[MusicEvent, Unit] = {
    case original @ TimeItemsEvent(timeItems) =>
      val initial = if(includeOriginal) original.timeItems else transformer.transform(original.timeItems)
      (0 until nrOfTransformations).foldLeft(initial) {
        case (items, i) =>
          listeners.takeItem().tell(TimeItemsEvent(items))
          transformer.transform(items)
      }
  }
}




