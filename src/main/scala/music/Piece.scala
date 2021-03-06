package music

import music.MusicActor._
import music.Pattern._
import music.SpectrumName._
import music.TimeItem._
import music.TwoPhasePatternFrequencyFilterBuilder.FrequencyBwsPattern

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
 * amp, attackTime, attackType
 *
 * FrequencyItem (Sound)
 * frequencies, bws, instrumentName
 *
 * PositionItem
 * startPan, endPan
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
 *
 * Grain
 * Vi får totaltid för ett grain
 * Vi hämtar ut en lista med TimeItem via pattern. Det finns ett antal fasta.
 * 2, 3, 2, 5, 3
 *
 *
 * För varje del så väljer vi ut ett mönster via pattern. Det finns ett antal fasta.
 * Dom har
 * - Delta tider.
 * - relativ duration eller fast duration
 * - Simple Gesture amp, attackTime, curve.
 *
 *
 * GrainItem
 * GrainGrid - The grid that holds the grain pattern
 * GrainGesture the grain gesture
 *
 *
 */

object Piece {
  final val totalDuration = TimeItemEvent(TimeItem(0, 60 * 5, 60 * 5, None))

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

  val effectItemPattern = LookupPattern(
    Map(
      1 -> EffectItem(mix = 0.05f, room = 0.25f, damp = 0.5f),
      2 -> EffectItem(mix = 0.07f, room = 0.70f, damp = 0.8f),
      3 -> EffectItem(mix = 0.08f, room = 0.5f, damp = 0.5f),
      4 -> EffectItem(mix = 0.3f, room = 0.3f, damp = 0.8f)
    ),
    cycle(atom(1), atom(2), atom(3), atom(4))
  )

  val musicChannelMaker = MusicChannelMaker(effectItemPattern = effectItemPattern)

  val playGrains = true
  val playEffects = false

  val musicChannelPlayer = MusicChannelPlayer(Music.player, 21, None, playGrains, playEffects)

  val positionItemPatterns: PatternItem[PatternItem[PositionItem]] = cycle(
    atom(palindrome(Elide.BOTH, atom(PositionItem(-1f, -0.9f)), atom(PositionItem(-0.9f, -0.8f)), atom(PositionItem(-0.8f, -0.7f)), atom(PositionItem(-0.7f, -0.6f)))),
    atom(palindrome(Elide.BOTH, atom(PositionItem(-0.5f, -0.4f)), atom(PositionItem(-0.4f, -0.3f)), atom(PositionItem(-0.3f, -0.2f)), atom(PositionItem(-0.2f, -0.1f)), atom(PositionItem(-0.1f, 0.0f)))),

    atom(palindrome(Elide.BOTH, atom(PositionItem(0.5f, 0.4f)), atom(PositionItem(0.4f, 0.3f)), atom(PositionItem(0.3f, 0.2f)), atom(PositionItem(0.2f, 0.1f)), atom(PositionItem(0.1f, 0.0f)))),
    atom(palindrome(Elide.BOTH, atom(PositionItem(1f, 0.9f)), atom(PositionItem(0.9f, 0.8f)), atom(PositionItem(0.8f, 0.7f)), atom(PositionItem(0.7f, 0.6f))))
  )

  val gestureItemPatterns: PatternItem[PatternItem[GestureItem]] = cycle(
    atom(palindrome(Elide.BOTH,
      atom(GestureItem(0.14f, SOFT_INVPHI_TIME.value, LINEAR, LINEAR)),
      atom(GestureItem(0.16f, HALF_TIME.value, SINE, SINE)),
      atom(GestureItem(0.18f, SHARP_INVPHI_TIME.value, WELCH, WELCH))
    )),

    atom(cycle(
      atom(GestureItem(0.16f, SOFT_INVPHI_TIME.value, WELCH, WELCH)),
      atom(GestureItem(0.18f, HALF_TIME.value, SINE, SINE)),
      atom(GestureItem(0.16f, SOFT_INVPHI_TIME.value, SINE, SINE)),
      atom(GestureItem(0.16f, SHARP_INVPHI_TIME.value, LINEAR, LINEAR)),
      atom(GestureItem(0.18f, HALF_TIME.value, WELCH, WELCH))
    ))
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

  val grainGridPattern = cycle(
    atom(relativeScaledTime(
      (1, 1, timeAtom),
      (2, 2, timeAtom),
      (3, 3, timeAtom),
      (2, 2, timeAtom),
      (2, 2, timeAtom),
      (1, 1, timeAtom),
      (1, 1, timeAtom),
      (3, 3, timeAtom)
    )),

    atom(relativeScaledTime(
      (2, 2, timeAtom),
      (2, 2, timeAtom),
      (21, 21, timeAtom),
      (1, 1, timeAtom),
      (1, 1, timeAtom),
      (8, 8, timeAtom),
      (5, 5, timeAtom),
      (13, 13, timeAtom),
      (1, 1, timeAtom),
      (1, 1, timeAtom),
      (3, 3, timeAtom),
      (3, 3, timeAtom),
      (2, 2, timeAtom),
      (1, 1, timeAtom)
    ))
  )

  val gestureTimePattern =
    cycle(
      atom(relativeScaledTime(
        (5, 5, TimeItemDurationBuilder(RelativeDuration(0.06f), Some('long))),
        (3, 3, TimeItemDurationBuilder(AbsoluteDuration(0.001f), Some('middle))),
        (5, 5, TimeItemDurationBuilder(RelativeDuration(0.04f), Some('middle))),
        (1, 1, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('middle))),
        (1, 1, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('short))),
        (2, 2, TimeItemDurationBuilder(AbsoluteDuration(0.001f), Some('middle))))),

      atom(relativeScaledTime(
        (13, 13, TimeItemDurationBuilder(RelativeDuration(0.05f), Some('long))),
        (1, 1, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('middle))),
        (2, 2, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('short))),
        (3, 3, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('middle))),
        (21, 21, TimeItemDurationBuilder(RelativeDuration(0.05f), Some('long))),
        (3, 3, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('middle))),
        (1, 1, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('short))),
        (2, 2, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('short))),
        (2, 2, TimeItemDurationBuilder(AbsoluteDuration(0.0001f), Some('middle)))))

    )
  val grainPatterns = Map(
    'long -> cycle(atom(RelativeDeltaGrain('long, 1.5f, 0.3f, WELCH)), constant(RelativeDeltaGrain('long, 1.3f, 0.7f, SINE))),
    'middle -> cycle(atom(RelativeDeltaGrain('middle, 1.4f, 0.5f, SINE)), atom(RelativeDeltaGrain('middle, 1.3f, 0.7f, WELCH))),
    'short -> cycle(atom(RelativeDeltaGrain('short, 1.3f, 0.1f, EXPONENTIAL)), atom(RelativeDeltaGrain('short, 1.2f, 0.2f, SQUARED))),
    'longsoft ->
      cycle(
        atom(RelativeDeltaGrain('longsoft, 0.014f, 0.7f, WELCH)),
        atom(RelativeDeltaGrain('longsoft, 0.015f, 0.5f, WELCH)),
        atom(RelativeDeltaGrain('longsoft, 0.014f, 0.3f, WELCH))
      ))

  val grainGestureBuilder = GrainGestureBuilder(grainGridPattern, gestureTimePattern, grainPatterns)


  val longGrainGridPattern = cycle(
    atom(relativeScaledTime((21, 21, timeAtom), (13, 13, timeAtom))),
    atom(relativeScaledTime((1, 1, timeAtom), (1, 1, timeAtom))),
    atom(relativeScaledTime((13, 13, timeAtom), (21, 21, timeAtom)))
  )

  val longGestureTimePattern =
    cycle(
      atom(relativeScaledTime(
        (21, 34, TimeItemDurationBuilder(RelativeDuration(1f), Some('longsoft))),
        (13, 13, TimeItemDurationBuilder(AbsoluteDuration(0.001f), Some('short)))
      )),
      atom(relativeScaledTime(
        (1, 1, TimeItemDurationBuilder(RelativeDuration(0.001f), Some('short))),
        (1, 2, TimeItemDurationBuilder(RelativeDuration(1f), Some('longsoft)))
      )),
      atom(relativeScaledTime(
        (13, 13, TimeItemDurationBuilder(RelativeDuration(0.001f), Some('short))),
        (21, 34, TimeItemDurationBuilder(RelativeDuration(1f), Some('longsoft)))
      ))
    )

  val longGrainGestureBuilder = GrainGestureBuilder(longGrainGridPattern, longGestureTimePattern, grainPatterns)

  val topActor = withActor(TimeItemBuilderActor(constant(builder))) {
    _.listen(TimeItemSplitterActor())
      .listen(TimeItemBuilderActor(subPattern))
      .listen(TimeItemsTransformerActor(PatternTimeItemTransformer(transformPattern)))
      .listen(MusicItemMaker(frequencyFilterBuilderPattern, positionItemPatterns, gestureItemPatterns, List(grainGestureBuilder, longGrainGestureBuilder)))
      .listen(musicChannelMaker)
      .listen(musicChannelPlayer)
  }

  def playRealtime(): Unit = {
    Music.player.startPlay()
    musicChannelPlayer.playLayers()
    topActor.tell(totalDuration)
  }

  def main(args: Array[String]) {
    playRealtime()
  }
}


