package music

import music.MusicActor._
import FrequencyFilter._

case class MusicItem(timeItem: TimeItem, chord: FrequencyFilterChord, position: PositionItem, gesture: GestureItem)

case class MusicItemsEvent(musicItems: List[MusicItem]) extends MusicEvent

case class MusicItemMaker(frequencyBuilderPattern: PatternItem[FrequencyFilterChordBuilder],
                          positionItemPatterns: PatternItem[PatternItem[PositionItem]],
                          gestureItemPatterns: PatternItem[PatternItem[GestureItem]],
                          var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  def receive = {
    case TimeItemsEvent(timeItems) =>
      val frequencyBuilder = frequencyBuilderPattern.takeItem()
      val positionItemPattern = positionItemPatterns.takeItem()
      val gestureItemPattern = gestureItemPatterns.takeItem()
      listeners.takeItem().tell(MusicItemsEvent(timeItems.map {
        timeItem =>
          MusicItem(timeItem,
            frequencyBuilder.buildFrequencyChord,
            positionItemPattern.takeItem(),
            gestureItemPattern.takeItem())
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
              TimeArgument(musicItem.timeItem.duration, musicItem.gesture.attackTime).arguments ++
              PanArgument(musicItem.position.start, musicItem.position.end).arguments ++
              AmpArgument(musicItem.gesture.amplitude).arguments ++
              AttackArgument3(Left(musicItem.gesture.attackCurve.name.name), Left(musicItem.gesture.decayCurve.name.name)).arguments ++
              makeFrequenciesArgument(musicItem.chord).arguments ++
              makeBwsArgument(musicItem.chord).arguments, absoluteTimeToMillis(musicItem.timeItem.start))

      }
  }


}