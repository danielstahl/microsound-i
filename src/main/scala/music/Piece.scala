package music

import music.MusicActor._
import music.TimeItem._
import java.awt.Graphics2D
import music.Patterns._

/**
 * Pattern
 * Pattern who generate a stream of data. E.g a flat structure. Patterns can be nested.
 *
 * ScaledTime
 * Generate a stream of time. E.g a flat structure. Can be nested.
 *
 * PulseScaledTime. Divides the totalTime into n equal steps.
 *
 * PatternScaledTime
 * The scaled time is a pattern.
 *
 * RelativeScaledTime. Divides the totalTime into relative steps. E.g steps
 * 2, 3, 5 will be a totalTime of 10.
 *
 * Vi behöver en strukter som är hiearkisk i  beskrivning och även fortsätter att vara det efter
 * generering.
 *
 * "PulsePhase"
 * Bör göras mer generell. Ska kunna ta in en ScaledTime, generera den och
 * sedan kunna "göra sitt". Filtrera, generera, expandera etc.
 *
 *
 * Vi kanske skulle göra hela motorn event-styrd.
 * Uppifrån och ner.
 * Man skickar event. Kanske med starttid, duration, kanal etc.
 *
 * Nedanstående. Man skickar ett event med totalDuration till structure.
 * Structure skickar sedan events om relativt skalad tid med starttid och duration. etc..
 * Man kan tänka sig att mottagaren är ett pattern vilket gör att man kan få olika
 * mottagare vid varje "event".
 *
 * Lite grann som Actor-modellen.
 *
 * MusicActor
 * tell eller receive
 *
 * MusicEvent
 *
 * Vi skulle kunna göra mottagning och skickning typad i implementationen så att man
 * säger vilken typ man tar emot och vilken man skickar.
 */

object Piece {
  final val totalDuration = TimeItemEvent(TimeItem(0, 60 * 30, 60 * 3, 0.5f))

  val printActor: MusicActorPattern = constant(PrinterActor)

  val defaultDuration: Pattern[(Float, Float), PatternItem[(Float, Float)]] = constant(1.0f, 0.5f)

  val structure: TimeItemBuilderPattern =
    constant(relativeScaledTime((2, timeAtom(defaultDuration)), (8, timeAtom(defaultDuration)), (13, timeAtom(defaultDuration)), (5, timeAtom(defaultDuration)), (3, timeAtom(defaultDuration))))

  def buildPart(pulses: Int, repeats: Int, phases: Int, phase: Float) = {
    withActor(TimeItemBuilderActor(constant(PulseTimeBuilder(pulses, timeAtom(defaultDuration))))) {
      _.listen(TimeItemsTransformerActor(ChainedTimeItemTransformer(PulseTransformer(repeats))))
        .listen(TimeItemsTransformerActor(ChainedTimeItemTransformer(ScaleTransformer(phase)), nrOfTransformations = phases, includeOriginal = true))
        .listen(PrinterActor)
    }
  }


  val pulses: MusicActorPattern =
    line(
      constant(buildPart(pulses = 2, repeats = 5, phases = 5, phase = 1.01f)),
      constant(buildPart(pulses = 13, repeats = 2, phases = 3, phase = 1.01f)),
      constant(buildPart(pulses = 8, repeats = 3, phases = 3, phase = 1.01f)),
      constant(buildPart(pulses = 3, repeats = 5, phases = 3, phase = 1.01f)),
      constant(buildPart(pulses = 5, repeats = 8, phases = 3, phase = 1.01f)),
      printActor)

  val structureActor = withActor(TimeItemBuilderActor(structure)) {
    _.listen(TimeItemSplitterActor())
      .listen(pulses)
  }


  def main(args: Array[String]) {
    structureActor.tell(totalDuration)
  }

  /*
  final val totalDuration = 60 * 3

  // The big picture
  val structure =
    relativeScaledTime((2, timeAtom), (8, timeAtom), (13, timeAtom), (5, timeAtom), (3, timeAtom))

  //TODO Pattern?
  val pulses = List(
    pulsePhase(pulseScaledTime(2), repeats = 5, phases = 5, phase = 0.01f),
    pulsePhase(pulseScaledTime(13), repeats = 2, phases = 3, phase = 0.01f),
    pulsePhase(pulseScaledTime(8), repeats = 3, phases = 3, phase = 0.01f),
    pulsePhase(pulseScaledTime(3), repeats = 5, phases = 3, phase = 0.01f),
    pulsePhase(pulseScaledTime(5), repeats = 8, phases = 3, phase = 0.01f)
  )

  // the pulses as Pattern.
  val pulseCycle =
    cycle(
      atom(pulsePhase(pulseScaledTime(2), repeats = 5, phases = 5, phase = 0.01f)),
      atom(pulsePhase(pulseScaledTime(13), repeats = 2, phases = 3, phase = 0.01f)),
      atom(pulsePhase(pulseScaledTime(8), repeats = 3, phases = 3, phase = 0.01f)),
      atom(pulsePhase(pulseScaledTime(3), repeats = 5, phases = 3, phase = 0.01f)),
      atom(pulsePhase(pulseScaledTime(5), repeats = 8, phases = 3, phase = 0.01f)))


  //TODO reimplement with pulseCycle as pattern
  def structureWithPulse(totalDuration: Float) = {
    var structStream = structure.scale(totalDuration).toStream
    var startTime = 0f
    pulses.map {
      pulsePhase =>
        val tempDuration = structStream.head
        structStream = structStream.tail
        val tempResult = pulsePhase.repeat(startTime, tempDuration)
        startTime += tempDuration
        tempResult
    }
  }

  def xpos(xtime: Float): Int = 10 + (xtime * 10).round

  def format(theValue: Number): String = "%4.2f" format theValue


  def plot(totalDuration: Float): (Graphics2D) => Unit = {
    val data: List[List[List[TimeItem]]] = structureWithPulse(totalDuration)
    (g: Graphics2D) => {
      var tempStruct = 1
      data.foreach {
        struct =>
          var tempPhase = 1
          struct.foreach {
            phase =>
              phase.foreach {
                case TimeItem(startTime, duration) =>
                  val ypos = 750 - (tempStruct * 100) - (tempPhase * 15)
                  val xstart = xpos(startTime)
                  val xend = xpos(startTime + duration)
                  g.drawLine(xstart, ypos, xstart, ypos + 10)
                  g.drawString(format(startTime), xstart + 10, ypos)
              }
              tempPhase += 1
          }
          tempStruct += 1
      }

    }



  }
  */


}
