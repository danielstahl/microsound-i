package music


import TwoPhasePatternFrequencyFilterBuilder._
import FrequencyFilter._
import music.MusicActor._
import music.Pattern.PatternType

case class FrequencyFilter(startFreq: Float, endFreq: Float, startBws: Float, endBws: Float)

object FrequencyFilter {
  def makeFrequencyFilters(start: FrequencyBws, end: FrequencyBws) = {
    val (_, _, _, _, result) =
      start.freqs.foldLeft((start.freqs, end.freqs, start.bws, end.bws, List[FrequencyFilter]())) {
        case ((sfs, efs, sbs, ebs, tmp), _) =>
          (sfs.tail, efs.tail, sbs.tail, ebs.tail, tmp ::: List(FrequencyFilter(sfs.head, efs.head, sbs.head, ebs.head)))
      }
    result
  }


  def makeFrequenciesArgument(chord: FrequencyFilterChord): FrequenciesArgument = {
    FrequenciesArgument(chord.filters.map(f => f.startFreq), chord.filters.map(f => f.endFreq))
  }

  def makeBwsArgument(chord: FrequencyFilterChord): BandwithsArgument = {
    BandwithsArgument(chord.filters.map(f => f.startBws), chord.filters.map(f => f.endBws))
  }

  def absoluteTimeToMillis(time: Float): Long = (time * 1000).round.toLong

  private val defaultBase = BaseArgument()
  private val defaultTime = TimeArgument(5f, 0.5f)

  def playFrequencyBws(frequencyBws: FrequencyBws, endFrequencyBws: Option[FrequencyBws] = None, instrument: InstrumentName)(implicit player: MusicPlayer): Unit = {
    println(s"playing $frequencyBws")
    val end = endFrequencyBws.getOrElse(frequencyBws)
    player.startPlay()
    player.sendNew(
      instrument.arguments ++
        defaultBase.arguments ++
        defaultTime.arguments ++
        FrequenciesArgument(frequencyBws.freqs, end.freqs).arguments ++
        BandwithsArgument(frequencyBws.bws, end.bws).arguments, 0)
    player.sender.close()
  }

  def playFrequencyBwsCombinations[T](item: T, data: Map[T, FrequencyBws], instrument: InstrumentName, delta: Float = 5f)(implicit player: MusicPlayer) {
    var start = 0f
    val startFreqBws = data(item)
    player.startPlay()
    data.keys.filter(k => k != item).foreach {
      key =>
        val endFreqBws = data(key)
        //println(s"$startFreqBws to $endFreqBws")
        println(s"playing $item to $key")
        player.sendNew(
          instrument.arguments ++
            defaultBase.arguments ++
            defaultTime.copy(dur = delta).arguments ++
            FrequenciesArgument(startFreqBws.freqs, endFreqBws.freqs).arguments ++
            BandwithsArgument(startFreqBws.bws, endFreqBws.bws).arguments, absoluteTimeToMillis(start))
        start += delta
    }
  }

  def playChord(chord: FrequencyFilterChord)(implicit player: MusicPlayer): Unit = {
    player.startPlay()
    player.sendNew(
    chord.instrument.arguments ++
      defaultBase.arguments ++
      defaultTime.arguments ++
      makeFrequenciesArgument(chord).arguments ++
      makeBwsArgument(chord).arguments, 0)
  }
}

case class FrequencyBws(freqs: Seq[Float], bws: Seq[Float])

case class FrequencyFilterChord(filters: Seq[FrequencyFilter], instrument: InstrumentName)

trait FrequencyFilterChordBuilder {
  def buildFrequencyChords(size: Int): List[FrequencyFilterChord]
  def buildFrequencyChord: FrequencyFilterChord = buildFrequencyChords(1).head
}

case class SimplePatternFrequencyFilterBuilder[K](pattern: PatternItem[K], chords: Map[K, FrequencyFilterChord]) extends FrequencyFilterChordBuilder {
  override def buildFrequencyChords(size: Int): List[FrequencyFilterChord] =
    (1 to size).map(i => chords(pattern.takeItem())).toList
}

case class TwoPhasePatternFrequencyFilterBuilder[K](phaseOnePattern: PhaseOnePattern, phaseTwo: Map[InstrumentName, PhaseTwoPattern]) extends FrequencyFilterChordBuilder {

  override def buildFrequencyChords(size: Int): List[FrequencyFilterChord] = {
    (1 to size).map { i =>
      val instrument = phaseOnePattern.takeItem()
      buildFrequencyChord(instrument, phaseTwo(instrument))
    }.toList
  }

  def buildFrequencyChord(instrument: InstrumentName, phaseTwoPattern: PhaseTwoPattern): FrequencyFilterChord = {
    val (startPattern, endPattern) = phaseTwoPattern
    FrequencyFilterChord(makeFrequencyFilters(startPattern.takeItem(), endPattern.takeItem()), instrument)
  }

}

object TwoPhasePatternFrequencyFilterBuilder {
  type PhaseOnePattern = PatternType[InstrumentName]
  type FrequencyBwsPattern = PatternType[FrequencyBws]
  type PhaseTwoPattern = (FrequencyBwsPattern, FrequencyBwsPattern)

}


case class SingleInstrumentPatternFrequencyFilter(instrument: InstrumentName, startPattern: FrequencyBwsPattern, endPattern: FrequencyBwsPattern) extends FrequencyFilterChordBuilder {
  override def buildFrequencyChords(size: Int): List[FrequencyFilterChord] =
    (1 to size).map { i =>
      FrequencyFilterChord(makeFrequencyFilters(startPattern.takeItem(), endPattern.takeItem()), instrument)
    }.toList
}

trait FrequencyFilterChordTransformer {
  def transform(chords: List[FrequencyFilterChord]): List[FrequencyFilterChord]
}


case class GenerateFrequencyFilterChordEvent(size: Int) extends MusicEvent

case class FrequencyFilterChordsEvent(chords: List[FrequencyFilterChord]) extends MusicEvent

case class FrequencyFilterChordBuilderActor(builderPattern: PatternItem[FrequencyFilterChordBuilder], var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  override protected def receive: PartialFunction[MusicEvent, Unit] = {
    case GenerateFrequencyFilterChordEvent(size) =>
      listeners.takeItem().tell(FrequencyFilterChordsEvent(builderPattern.takeItem().buildFrequencyChords(size)))
  }
}


case class FrequencyFilterChordsPlayer(player: MusicPlayer, delta: Float) extends LeafActor {
  private val defaultBase = BaseArgument()
  private val defaultTime = TimeArgument(delta, 0.5f)

  override protected def receive: PartialFunction[MusicEvent, Unit] = {
    case FrequencyFilterChordsEvent(chords) =>
      var start = 0f
      chords.foreach {
        chord =>
          player.sendNew(
            chord.instrument.arguments ++
              defaultBase.arguments ++
              defaultTime.copy(dur = delta).arguments ++
              makeFrequenciesArgument(chord).arguments ++
              makeBwsArgument(chord).arguments, absoluteTimeToMillis(start))
          start += delta
      }
  }


}


