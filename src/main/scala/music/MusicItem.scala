package music

import music.MusicActor._
import FrequencyFilter._

case class MusicItem(timeItem: TimeItem, chord: FrequencyFilterChord)

case class MusicItemsEvent(musicItems: List[MusicItem]) extends MusicEvent

case class MusicItemMaker(frequencyFilterChordBuilderPattern: PatternItem[FrequencyFilterChordBuilder], var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  def receive = {
    case TimeItemsEvent(timeItems) =>
      listeners.takeItem().tell(MusicItemsEvent(timeItems.map {
        timeItem =>
          MusicItem(timeItem, frequencyFilterChordBuilderPattern.takeItem().buildFrequencyChord)
      }))
  }
}

case class MusicChannel(channel: Int, musicItems: List[MusicItem])

case class MusicChannelEvent(musicChannel: MusicChannel) extends MusicEvent

case class MusicChannelMaker(var listeners: MusicActorPattern = emptyActor)  extends NodeActor {
  var channel = 0
  def receive = {
    case MusicItemsEvent(musicItems) =>
      listeners.takeItem().tell(MusicChannelEvent(MusicChannel(channel , musicItems)))
      channel = channel + 1
  }
}


case class MusicChannelPlayer(player: MusicPlayer) extends LeafActor {
  private val defaultBase = BaseArgument()

  override protected def receive: PartialFunction[MusicEvent, Unit] = {
    case MusicChannelEvent(MusicChannel(channel, musicItems)) =>
      musicItems.foreach {
        musicItem =>
          println(s"playing $musicItem")
          player.sendNew(
            musicItem.chord.instrument.arguments ++
              defaultBase.arguments ++
              TimeArgument(musicItem.timeItem.duration, 0.5f).arguments ++
              makeFrequenciesArgument(musicItem.chord).arguments ++
              makeBwsArgument(musicItem.chord).arguments, absoluteTimeToMillis(musicItem.timeItem.start))

      }
  }


}