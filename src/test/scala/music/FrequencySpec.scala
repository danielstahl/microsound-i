package music

import music.NoteName._
import music.TwoPhasePatternFrequencyFilterBuilder.{FrequencyBwsPattern, PhaseTwoPattern, PhaseOnePattern}

import org.scalatest.FlatSpec
import Pattern._

class FrequencySpec extends FlatSpec {
  behavior of "PatternFrequencyFilterBuilder"

  it should "return size number of chords from pattern" in {

    val pattern: Pattern[String, PatternItem[String]] = line(atom("two"), atom("one"))
    val chords = Map(
      "one" -> FrequencyFilterChord(Seq(FrequencyFilter(100f, 100f, 0.01f, 0.01f)), InvertedSpektrum4),
      "two" -> FrequencyFilterChord(Seq(FrequencyFilter(200f, 200f, 0.02f, 0.02f)), InvertedSpektrum4)
    )

    val builder = SimplePatternFrequencyFilterBuilder(pattern, chords)
    val expectedResult = List(chords("two"), chords("one"), chords("one"))
    assert(builder.buildFrequencyChords(3) === expectedResult)
  }

  it should "work with enums as keys" in {
    val pattern: Pattern[NoteName, PatternItem[NoteName]] = line(atom(noise2), atom(noise1))
    val chords = Map(
      noise1 -> FrequencyFilterChord(Seq(FrequencyFilter(100f, 100f, 0.01f, 0.01f)), InvertedSpektrum4),
      noise2 -> FrequencyFilterChord(Seq(FrequencyFilter(200f, 200f, 0.02f, 0.02f)), InvertedSpektrum4)
    )

    val builder = SimplePatternFrequencyFilterBuilder(pattern, chords)
    val expectedResult = List(chords(noise2), chords(noise1), chords(noise1))
    assert(builder.buildFrequencyChords(3) === expectedResult)
  }

  behavior of "TwoPhasePatternFrequencyFilterBuilder"

  it should "make chords out of two step patterns" in {
    val phaseOnePattern: PhaseOnePattern = constant(InvertedSpektrum4)

    val phaseTwoLookup = Map(
      'one -> FrequencyBws(Seq(100, 200, 300, 400), Seq(1, 1, 1, 1)),
      'two -> FrequencyBws(Seq(1000, 2000, 3000, 4000), Seq(10, 10, 10, 10)))

    val phaseTwoPattern: PhaseTwoPattern = (LookupPattern(phaseTwoLookup, constant('one)), LookupPattern(phaseTwoLookup, constant('two)))
    val phaseTwo: Map[InstrumentName, PhaseTwoPattern] = Map(InvertedSpektrum4 -> phaseTwoPattern)
    val builder = TwoPhasePatternFrequencyFilterBuilder(phaseOnePattern, phaseTwo)
    val expectedResult =
      List(FrequencyFilterChord(Seq(
        FrequencyFilter(100f, 1000f, 1f, 10f),
        FrequencyFilter(200f, 2000f, 1f, 10f),
        FrequencyFilter(300f, 3000f, 1f, 10f),
        FrequencyFilter(400f, 4000f, 1f, 10f)), InvertedSpektrum4))
    assert(builder.buildFrequencyChords(1) == expectedResult)
  }


}
