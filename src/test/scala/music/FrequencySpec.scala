package music

import music.NoteName._

import org.scalatest.FlatSpec
import Patterns._

class FrequencySpec extends FlatSpec {
  behavior of "PatternFrequencyFilterBuilder"

  it should "return size number of chords from pattern" in {

    val pattern: Pattern[String, PatternItem[String]] = line(atom("two"), atom("one"))
    val chords = Map(
      "one" -> FrequencyFilterChord(Seq(FrequencyFilter(100f, 100f, 0.01f, 0.01f)), InvertedSpektrum4),
      "two" -> FrequencyFilterChord(Seq(FrequencyFilter(200f, 200f, 0.02f, 0.02f)), InvertedSpektrum4)
    )

    val builder = PatternFrequencyFilterBuilder(pattern, chords)
    val expectedResult = List(chords("two"), chords("one"), chords("one"))
    assert(builder.buildFrequencyChords(3) === expectedResult)
  }

  it should "work with enums as keys" in {
    val pattern: Pattern[NoteName, PatternItem[NoteName]] = line(atom(noise2), atom(noise1))
    val chords = Map(
      noise1 -> FrequencyFilterChord(Seq(FrequencyFilter(100f, 100f, 0.01f, 0.01f)), InvertedSpektrum4),
      noise2 -> FrequencyFilterChord(Seq(FrequencyFilter(200f, 200f, 0.02f, 0.02f)), InvertedSpektrum4)
    )

    val builder = PatternFrequencyFilterBuilder(pattern, chords)
    val expectedResult = List(chords(noise2), chords(noise1), chords(noise1))
    assert(builder.buildFrequencyChords(3) === expectedResult)
  }
}
