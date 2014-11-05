package music

/**
 * The grain item.
 * - Grain grid
 * - Grain gesture
 *
 *
 * Grain grid should be possible to build with TimeItemBuilder
 *
 * PatternItem[TimeItemBuilder]
 */

object GrainItem {
}

/**
 * This is a relative grain
 *
 * The relative delta time denotes part of the
 * whole gesture, e.g much like the relativeTimeBuilder
 *
 * If the duration is relative it is relative to the
 * absolute delta time. The attack time is relative
 * to the duration.
 */
case class RelativeDeltaGrain(noteClass: Symbol, amplitude: Float, attackTime: Float, curve: EnvCurve)

/**
 * The absolute grain is the finished grain.
 * The start is the startTime. The delta, duration, attackTime is all relative to
 * the start time.
 */
case class AbsoluteGrain(var start: Float, delta: Float, duration: Float, amplitude: Float, attackTime: Float, curve: EnvCurve)

/**
 * This builder builds a gesture with grains. The time is taken from a
 * TimeItemBuilder. The grainGridPattern build the grid and the gestgureItemPattern
 * builds the grain gesture.
 */
case class GrainGestureBuilder(grainGridPattern: PatternItem[TimeItemBuilder],
                               gestureTimePattern: PatternItem[TimeItemBuilder],
                               grainPatterns: Map[Symbol, PatternItem[RelativeDeltaGrain]]) {

  def buildGrainGestures(start: Float, delta: Float, duration: Float): List[AbsoluteGrain] = {
    val grainGrid = grainGridPattern.takeItem().build(start, delta, duration)
    grainGrid.flatMap {
      gridItem =>
        val gestureTime = gestureTimePattern.takeItem().build(gridItem.start, gridItem.delta, gridItem.duration)
        gestureTime.map {
          grainTime =>
            val noteClass = grainTime.noteClass.getOrElse(sys.error("You have to supply a noteclass"))
            val deltaGrain = grainPatterns(noteClass).takeItem()
            AbsoluteGrain(grainTime.start, grainTime.delta, grainTime.duration, deltaGrain.amplitude, deltaGrain.attackTime, deltaGrain.curve)
        }
    }
  }
}



