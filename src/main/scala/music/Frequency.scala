package music


case class FrequencyFilter(startFreq: Float, endFreq: Float, startBws: Float, endBws: Float)

case class FrequencyFilterChord(filters: Seq[FrequencyFilter])

trait FrequencyFilterChordBuilder {
  def buildFrequencyChords(size: Int): List[FrequencyFilterChord]
}

case class PatternFrequencyFilterBuilder(pattern: Pattern[String, PatternItem[String]], chords: Map[String, FrequencyFilterChord]) extends FrequencyFilterChordBuilder {
  override def buildFrequencyChords(size: Int): List[FrequencyFilterChord] =
    (1 to size).map(i => chords(pattern.takeItem())).toList
}

trait FrequencyFilterChordTransformer {
  def transform(chords: List[FrequencyFilterChord]): List[FrequencyFilterChord]
}



