package music

import music.MusicActor._
import music.TimeItem._

case class TimeItem(start: Float, delta: Float, duration: Float)

object TimeItem {
  type TimeItemBuilderPattern = PatternItem[TimeItemBuilder]
  def timeAtom = TimeAtomBuilder

  def patternScaledTime(pattern: TimeItemBuilderPattern) =
    PatternTimeItemBuilder(pattern)

  def pulseScaledTime(steps: Int, scaledTime: TimeItemBuilder) =
    PulseTimeBuilder(steps, scaledTime)

  def relativeScaledTime(parts: (Int, Int, TimeItemBuilder)*) =
    RelativeTimeBuilder(parts.toList)


}

case class DeltaTime(delta: Float, duration: Float)

trait TimeItemBuilder {
  def build(startTime: Float, totalDelta: Float, totalDuration: Float): List[TimeItem] = {
    buildAbsoluteTime(startTime, buildRelativeTime(totalDelta, totalDuration))
  }

  def buildRelativeTime(totalDelta: Float, totalDuration: Float): List[DeltaTime]

  protected def buildAbsoluteTime(startTime: Float, parts: List[DeltaTime]): List[TimeItem] = {
    var tempTime = startTime
    parts.map {
      deltaTime =>
        val item = TimeItem(tempTime, deltaTime.delta, deltaTime.duration)
        tempTime = tempTime + deltaTime.delta
        item
    }
  }
}

object TimeAtomBuilder extends TimeItemBuilder {
  override def buildRelativeTime(totalDelta: Float, totalDuration: Float): List[DeltaTime] = {
    List(DeltaTime(totalDelta, totalDuration))
  }
}

case class PatternTimeItemBuilder(pattern: TimeItemBuilderPattern) extends TimeItemBuilder {
  override def buildRelativeTime(totalDelta: Float, totalDuration: Float): List[DeltaTime] = pattern.takeItem().buildRelativeTime(totalDelta, totalDuration)
}

case class PulseTimeBuilder(steps: Int, scaledTime: TimeItemBuilder) extends TimeItemBuilder {
  def buildRelativeTime(totalDelta: Float, totalDuration: Float): List[DeltaTime] = {
    val deltaFact = totalDelta / steps.toFloat
    val durationFact = totalDuration / steps.toFloat
    (1 to steps).map(i => scaledTime.buildRelativeTime(deltaFact, durationFact)).flatten.toList
  }
}

case class RelativeTimeBuilder(parts: List[(Int, Int, TimeItemBuilder)]) extends TimeItemBuilder {
  def buildRelativeTime(totalDelta: Float, totalDuration: Float): List[DeltaTime] = {
    val deltaFact = totalDelta / parts.map(_._1).sum.toFloat
    val durationFact = totalDuration / parts.map(_._2).sum.toFloat
    parts.map {
      case (deltaPart, durationPart, scaledTime) => scaledTime.buildRelativeTime(deltaPart * deltaFact, durationPart * durationFact)
    }.flatten.toList
  }
}

case class RelativeTimeMetaBuilder(parts: PatternItem[(Int, Int, TimeItemBuilder)]) {
  def buildBuilder(size: Int): RelativeTimeBuilder =
    RelativeTimeBuilder((1 to size).map(i => parts.takeItem()).toList)
}


trait TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem]
}

case class PulseTransformer(repeats: Int = 1) extends TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem] = {
    val totalDelta = items.map(_.delta).sum
    items ++ (1 to repeats).flatMap {
      i =>
       val startTime = i * totalDelta
       items.map {
         case TimeItem(start, delta, duration) => TimeItem(startTime + start, delta, duration)
       }
    }
  }
}

case class ScaleTransformer(deltaFactor: Float = 1, durationFactor: Float = 1) extends TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem] = {
    val startTime = items.head.start

    val newItems = items.foldLeft((startTime, List[TimeItem]())) {
      case ((nextStart, result), timeItem) =>
        val newDelta = timeItem.delta * deltaFactor
        val newDuration = timeItem.duration * durationFactor
        (nextStart + newDelta, result ::: List(TimeItem(nextStart, newDelta, newDuration)))
    }
    newItems._2
  }
}

case class ChainedTimeItemTransformer(transformers: TimeItemTransformer*) extends TimeItemTransformer {
  override def transform(items: List[TimeItem]): List[TimeItem] = {
    transformers.foldLeft(items) {
      case (theItems, transformer) => transformer.transform(theItems)
    }
  }
}

case class PatternTimeItemTransformer(pattern: PatternItem[TimeItemTransformer]) extends TimeItemTransformer {
  override def transform(items: List[TimeItem]): List[TimeItem] = {
    pattern.takeItem().transform(items)
  }
}


case class TimeItemEvent(timeItem: TimeItem) extends MusicEvent

case class TimeItemBuilderActor(timeItemBuilders: TimeItemBuilderPattern, var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  def receive = {
    case TimeItemEvent(timeItem) =>
      val timeItems = timeItemBuilders.takeItem().build(timeItem.start, timeItem.delta, timeItem.duration)
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

case class TimeItemPlotterActor(dataPlotter: DataPlotterFx) extends LeafActor {
  var channel = 0
  def receive = {
    case event @ TimeItemsEvent(timeItems) =>
      event.channel = channel
      channel = channel + 1
      timeItems.foreach {
        timeItem =>
          dataPlotter.plot {
            gc =>
              val ypos = 20d + (event.channel * 20d)
              val startPos = xpos(timeItem.start)
              val durationEndPos = xpos(timeItem.start + timeItem.duration)
              gc.strokeLine(startPos.toDouble, ypos, durationEndPos.toDouble, ypos)
              gc.strokeLine(startPos.toDouble, ypos, startPos.toDouble, ypos - 10)
              val timeString = f"${timeItem.start}%.2f"
              gc.strokeText(timeString, startPos.toDouble + 5, ypos - 5)

          }
      }
  }

  def xpos(xtime: Float): Double = 10 + (xtime * 10).round
}