package music


case class FrequencyFilter(startFreq: Float, endFreq: Float, startBws: Float, endBws: Float)

case class FrequencyFilterChord(filters: Seq[FrequencyFilter], instrument: InstrumentName)

trait FrequencyFilterChordBuilder {
  def buildFrequencyChords(size: Int): List[FrequencyFilterChord]
}

case class PatternFrequencyFilterBuilder[K](pattern: Pattern[K, PatternItem[K]], chords: Map[K, FrequencyFilterChord]) extends FrequencyFilterChordBuilder {
  override def buildFrequencyChords(size: Int): List[FrequencyFilterChord] =
    (1 to size).map(i => chords(pattern.takeItem())).toList
}

trait FrequencyFilterChordTransformer {
  def transform(chords: List[FrequencyFilterChord]): List[FrequencyFilterChord]
}



