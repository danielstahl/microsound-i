package music

case class TimeItem(start: Float, delta: Float, duration: Float, attack: Float)

object TimeItem {
  def timeAtom(pattern: Pattern[(Float, Float), PatternItem[(Float, Float)]]): TimeAtomBuilder = TimeAtomBuilder(pattern)

  def patternScaledTime(pattern: Pattern[TimeItemBuilder, PatternItem[TimeItemBuilder]]) =
    PatternTimeItemBuilder(pattern)

  def pulseScaledTime(steps: Int, scaledTime: TimeItemBuilder) =
    PulseTimeBuilder(steps, scaledTime)

  def relativeScaledTime(parts: (Int, TimeItemBuilder)*) =
    RelativeTimeBuilder(parts.toList)
}

case class DeltaTime(delta: Float, duration: Float, attack: Float)

trait TimeItemBuilder {
  def build(startTime: Float, totalDuration: Float): List[TimeItem] = {
    buildAbsoluteTime(startTime, buildRelativeTime(totalDuration))
  }

  def buildRelativeTime(totalDuration: Float): List[DeltaTime]

  protected def buildAbsoluteTime(startTime: Float, parts: List[DeltaTime]): List[TimeItem] = {
    var tempTime = startTime
    parts.map {
      deltaTime =>
        val item = TimeItem(tempTime, deltaTime.delta, deltaTime.duration, deltaTime.attack)
        tempTime = tempTime + deltaTime.delta
        item
    }
  }
}

case class TimeAtomBuilder(pattern: Pattern[(Float, Float), PatternItem[(Float, Float)]]) extends TimeItemBuilder {
  override def buildRelativeTime(totalDuration: Float): List[DeltaTime] = {
    val (deltaDuration, attack) = pattern.takeItem()
    List(DeltaTime(totalDuration, totalDuration * deltaDuration, attack))
  }
}

case class PatternTimeItemBuilder(pattern: Pattern[TimeItemBuilder, PatternItem[TimeItemBuilder]]) extends TimeItemBuilder {
  override def buildRelativeTime(totalDuration: Float): List[DeltaTime] = pattern.takeItem().buildRelativeTime(totalDuration)
}

case class PulseTimeBuilder(steps: Int, scaledTime: TimeItemBuilder) extends TimeItemBuilder {
  def buildRelativeTime(totalDuration: Float): List[DeltaTime] = {
    val fact = totalDuration / steps.toFloat
    (1 to steps).map(i => scaledTime.buildRelativeTime(fact)).flatten.toList
  }
}

case class RelativeTimeBuilder(parts: List[(Int, TimeItemBuilder)]) extends TimeItemBuilder {
  def buildRelativeTime(totalDuration: Float): List[DeltaTime] = {
    val fact = totalDuration / parts.map(_._1).sum.toFloat
    parts.map {
      case (part, scaledTime) => scaledTime.buildRelativeTime(part * fact)
    }.flatten.toList
  }
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
         case TimeItem(start, delta, duration, attack) => TimeItem(startTime + start, delta, duration, attack)
       }
    }
  }
}

case class ScaleTransformer(deltaFactor: Float = 1, durationFactor: Float = 1, attackFactor: Float = 1) extends TimeItemTransformer {
  def transform(items: List[TimeItem]): List[TimeItem] = {
    val startTime = items.head.start

    val newItems = items.foldLeft((startTime, List[TimeItem]())) {
      case ((nextStart, result), timeItem) => {
        val newDelta = timeItem.delta * deltaFactor
        val newDuration = timeItem.duration * durationFactor
        val newAttack = timeItem.attack * attackFactor
        (nextStart + newDelta, result ::: List(TimeItem(nextStart, newDelta, newDuration, newAttack)))
      }
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

case class PatternTimeItemTransformer(pattern: Pattern[TimeItemTransformer, PatternItem[TimeItemTransformer]]) extends TimeItemTransformer {
  override def transform(items: List[TimeItem]): List[TimeItem] = {
    pattern.takeItem().transform(items)
  }
}

