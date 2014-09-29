package music

import music.MusicActor._
import music.TimeItem._
import java.awt.Graphics2D
import music.Pattern._
import music.TwoPhasePatternFrequencyFilterBuilder.{FrequencyBwsPattern, PhaseTwoPattern, PhaseOnePattern}
import SpectrumName._

/**
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
 *
 *
 *
 *
 * TimeItem
 * startTime, delta, duration
 *
 * GestureItem
 * attackTime, attackType
 *
 * FrequencyItem (Sound)
 * frequencies, bws, instrumentName
 *
 * PositionItem
 * startPan, endPan
 *
 *
 *
 *
 *
 * Toppstrukturen
 * Generera fler stek i toppen
 *
 * En flerstegs lookup-pattern.
 * 1. Del av helhet, tex 13
 * 2. Pulse tex (pulse 2, pattern 3 2 2). Generera båda delta och duration.
 * 3. Repeat av ovanstående pulse eller pattern samt transformation. Både
 * delta och duration.
 * 4. Nu får vi en lista med TimeItem
 *
 *
 *
 *
 *
 *
 * Regarding Pattern
 * We should expose PatternItem[A] not Pattern
 *
 *
 */

object Piece {
  final val totalDuration = TimeItemEvent(TimeItem(0, 60 * 5, 60 * 5))

  val printActor: MusicActorPattern = constant(PrinterActor)

  val plotter = DataPlotterFx()
  val plotterActor = TimeItemPlotterActor(plotter)


  val chord1 = Harmony(HARMON).octave(5).chord(0, 1, 2, 3, 4, 5)
  val bandwith1 = Seq(10f, .1f, .1f, .1f, .1f, 10f)
  val frequencyBws1 = FrequencyBws(chord1, bandwith1)
  val chord2 = Harmony(HARMON).octave(5).chord(10, 11, 12, 13, 14, 15)
  val bandwith2 = Seq(10f, .01f, .01f, .01f, .01f, 10f)
  val frequencyBws2 = FrequencyBws(chord2, bandwith2)
  val chord3 = Harmony(HARMON).octave(4).chord(2, 4, 5, 8, 10, 12)
  val bandwith3 = Seq(10f, 0.01f, 0.1f, 0.0f, 0.1f, 10f)
  val frequencyBws3 = FrequencyBws(chord3, bandwith3)
  val chord4 = Harmony(HARMON).octave(8).chord(0, 3, 6, 9, 12, 15)
  val bandwith4 = Seq(10f, 0.01f, 0.001f, 0.0001f, 0.00001f, 10f)
  val frequencyBws4 = FrequencyBws(chord4, bandwith4)
  val chord5 = Harmony(PHI).octave(3).chord(0, 1, 2, 3, 4, 5)
  val bandwith5 = Seq(10f, 0.02f, 0.07f, 0.02f, 0.02f, 10f)
  val frequencyBws5 = FrequencyBws(chord5, bandwith5)
  val chord6 = Harmony(PHI).octave(5).chord(0, 1, 2, 3, 4, 5)
  val bandwith6 = Seq(10f, 0.000002f, 0.000007f, 0.000004f, 0.000003f, 10f)
  val frequencyBws6 = FrequencyBws(chord6, bandwith6)
  val chord7 = Harmony(PHI).octave(5).chord(0, 1, 2, 3, 4, 5)
  val bandwith7 = Seq(10f, 0.002f, 0.007f, 0.0004f, 0.0003f, 10f)
  val frequencyBws7 = FrequencyBws(chord7, bandwith7)

  val frequencyBwsData = Map(
    'noise1 -> frequencyBws1,
    'noise2 -> frequencyBws2,
    'noise3 -> frequencyBws3,
    'noise4 -> frequencyBws4,
    'noise5 -> frequencyBws5,
    'noise6 -> frequencyBws6,
    'noise7 -> frequencyBws7
  )

  def frequencyFilterBuilder(startPattern: FrequencyBwsPattern, endPattern: FrequencyBwsPattern) =
    SingleInstrumentPatternFrequencyFilter(InvertedSpektrum6, startPattern, endPattern)

  val frequencyFilterBuilder1 =
    frequencyFilterBuilder(
      LookupPattern(frequencyBwsData, cycle(atom('noise1), atom('noise2), atom('noise3))),
      LookupPattern(frequencyBwsData, palindrome(Elide.BOTH, atom('noise3), atom('noise2), atom('noise1))))

  val frequencyFilterBuilder2 =
    frequencyFilterBuilder(
      LookupPattern(frequencyBwsData, palindrome(Elide.BOTH, atom('noise4), atom('noise5), atom('noise6))),
      LookupPattern(frequencyBwsData, cycle(atom('noise7), atom('noise6), atom('noise5)))
    )

  val frequencyFilterBuilder3 =
    frequencyFilterBuilder(
      LookupPattern(frequencyBwsData, cycle(atom('noise3), atom('noise2), atom('noise1))),
      LookupPattern(frequencyBwsData, palindrome(Elide.BOTH, atom('noise1), atom('noise2), atom('noise3))))

  val frequencyFilterBuilder4 =
    frequencyFilterBuilder(
      LookupPattern(frequencyBwsData, palindrome(Elide.BOTH, atom('noise6), atom('noise5), atom('noise4))),
      LookupPattern(frequencyBwsData, cycle(atom('noise5), atom('noise6), atom('noise7)))
    )

  val frequencyFilterBuilderPattern = cycle(atom(frequencyFilterBuilder1), atom(frequencyFilterBuilder2), atom(frequencyFilterBuilder3), atom(frequencyFilterBuilder4))

  val frequencyFilterChordsPlayer = FrequencyFilterChordsPlayer(Music.player, 5)

  val frequencyFilterBuilderActor =
    withActor(FrequencyFilterChordBuilderActor(constant(frequencyFilterBuilder4))) {
      _.listen(frequencyFilterChordsPlayer)
    }

  val musicChannelMaker = MusicChannelMaker()

  val musicChannelPlayer = MusicChannelPlayer(Music.player)

  val positionItemPatterns: PatternItem[PatternItem[PositionItem]] = cycle(
    atom(palindrome(Elide.BOTH, atom(PositionItem(-1f, -0.9f)), atom(PositionItem(-0.9f, -0.8f)), atom(PositionItem(-0.8f, -0.7f)), atom(PositionItem(-0.7f, -0.6f)))),
    atom(palindrome(Elide.BOTH, atom(PositionItem(-0.5f, -0.4f)), atom(PositionItem(-0.4f, -0.3f)), atom(PositionItem(-0.3f, -0.2f)), atom(PositionItem(-0.2f, -0.1f)), atom(PositionItem(-0.1f, 0.0f)))),

    atom(palindrome(Elide.BOTH, atom(PositionItem(0.5f, 0.4f)), atom(PositionItem(0.4f, 0.3f)), atom(PositionItem(0.3f, 0.2f)), atom(PositionItem(0.2f, 0.1f)), atom(PositionItem(0.1f, 0.0f)))),
    atom(palindrome(Elide.BOTH, atom(PositionItem(1f, 0.9f)), atom(PositionItem(0.9f, 0.8f)), atom(PositionItem(0.8f, 0.7f)), atom(PositionItem(0.7f, 0.6f))))
  )

  val pattern = cycle(
    atom((2, 1, timeAtom)),
    palindrome(Elide.LAST, atom((3, 2, timeAtom)), atom((2, 2, timeAtom))),
    cycle(atom((5, 3, timeAtom)), atom((3, 3, timeAtom))),
    cycle(atom((8, 13, timeAtom)), atom((8, 5, timeAtom))),
    cycle(atom((13, 8, timeAtom)), atom((5, 5, timeAtom)))
  )

  val subPattern: PatternItem[TimeItemBuilder] = cycle(
    atom(relativeScaledTime((3, 3, timeAtom), (5, 5, timeAtom))),
    atom(pulseScaledTime(3, timeAtom)),
    atom(relativeScaledTime((2, 2, timeAtom), (2, 2, timeAtom), (3, 3, timeAtom))),
    atom(pulseScaledTime(5, timeAtom)),
    atom(pulseScaledTime(2, timeAtom))
  )
  val metaBuilder = RelativeTimeMetaBuilder(pattern)
  val builder = metaBuilder.buildBuilder(21)
  val transformPattern: PatternItem[TimeItemTransformer] =
    cycle(
      atom(PulseTransformer(3)),
      atom(PulseTransformer(2)),
      atom(PulseTransformer(5))
    )

  val topActor = withActor(TimeItemBuilderActor(constant(builder))) {
    _.listen(TimeItemSplitterActor())
      .listen(TimeItemBuilderActor(subPattern))
      .listen(TimeItemsTransformerActor(PatternTimeItemTransformer(transformPattern)))
      .listen(MusicItemMaker(frequencyFilterBuilderPattern, positionItemPatterns))
      .listen(musicChannelMaker)
      .listen(musicChannelPlayer)
  }



  def main(args: Array[String]) {
    //Music.player.startPlay()
    //plotter.show()
    //structureActorv2.tell(totalDuration)

    Music.player.startPlay()
    //frequencyFilterBuilderActor.tell(GenerateFrequencyFilterChordEvent(10))
    //val result = frequencyFilterBuilder.buildFrequencyChords(12)
    //result.foreach(println(_))

    topActor.tell(totalDuration)
  }




}


import SpectrumName._

sealed trait LNote {
  val freq: FrequencyFilterChord

  def chord(instrument: InstrumentName, startFreqs: Seq[Float], endFreqs: Seq[Float], startBws: Seq[Float], endBws: Seq[Float]) = {
    FrequencyFilterChord(
      makeFrequencyFilters(
        startFreqs, endFreqs,
        startBws, endBws), instrument)
  }

  def makeFrequencyFilters(startFreqs: Seq[Float], endFreqs: Seq[Float], startBws: Seq[Float], endBws: Seq[Float]) = {
    val (_, _, _, _, result) =
      startFreqs.foldLeft((startFreqs, endFreqs, startBws, endBws, List[FrequencyFilter]())) {
      case ((sfs, efs, sbs, ebs, tmp), _) =>
        (sfs.tail, efs.tail, sbs.tail, ebs.tail, tmp ::: List(FrequencyFilter(sfs.head, efs.head, sbs.head, ebs.head)))
    }
    result
  }
}

object Noise1 extends LNote {
  val freq: FrequencyFilterChord =
    chord(InvertedSpektrum4,
        Harmony(HARMON).octave(2).chord(1, 7, 15, 18),
        Harmony(HARMON).octave(2).chord(1, 6, 12, 19),
        Bandwith(2, 2, 2, 2),
        Bandwith(0, 0, 0, 0))
}
