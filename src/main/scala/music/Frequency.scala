package music


case class FrequencyFilter(startFreq: Float, endFreq: Float, startBws: Float, endBws: Float)

case class FrequencyFilterChord(filters: Seq[FrequencyFilter])

trait FrequencyFilterChordBuilder {
  def buildFrequencyChords(size: Int): List[FrequencyFilterChord]
}

trait FrequencyFilterChordTransformer {
  def transform(chords: List[FrequencyFilterChord]): List[FrequencyFilterChord]
}

