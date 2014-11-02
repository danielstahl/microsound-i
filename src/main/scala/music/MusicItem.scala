package music

import music.MusicActor._
import FrequencyFilter._

case class MusicItem(timeItem: TimeItem, chord: FrequencyFilterChord, position: PositionItem, gesture: GestureItem, grains: List[AbsoluteGrain])

case class MusicItemsEvent(musicItems: List[MusicItem]) extends MusicEvent

case class MusicItemMaker(frequencyBuilderPattern: PatternItem[FrequencyFilterChordBuilder],
                          positionItemPatterns: PatternItem[PatternItem[PositionItem]],
                          gestureItemPatterns: PatternItem[PatternItem[GestureItem]],
                          grainGestureBuilder: GrainGestureBuilder,
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
            gestureItemPattern.takeItem(),
            grainGestureBuilder.buildGrainGestures(timeItem.start, timeItem.delta, timeItem.duration))
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

case class MusicChannelPlayer(player: MusicPlayer, nbrOfChannels: Int, channelsToPlay: Option[List[Int]] = None, playGrains: Boolean = false) extends LeafActor {
  private val defaultBase = BaseArgument()

  val layers = if(playGrains) Some(Layers(nbrOfChannels)) else None

  def playLayers(): Unit = {
    implicit val p = player
    layers.foreach {
      layers =>
        layers.play()
    }
  }

  override protected def receive: PartialFunction[MusicEvent, Unit] = {
    case MusicChannelEvent(MusicChannel(channel, musicItems)) =>
      musicItems.foreach {
        musicItem =>
          if(shouldPlayItem(channel)) {
            updateStartDelta(musicItem, channel)
            updateStartTime(musicItem)
            playMusicItem(musicItem, channel, playGrains)
            if(playGrains)
              musicItem.grains.foreach {
                grain =>
                  updateStartTime(grain)
                  playGrain(grain, channel)
              }

          }
      }
  }

  var startDelta: Option[Float] = None

  def updateStartDelta(musicItem: MusicItem, channel: Int): Unit = {
    if(channelsToPlay.isDefined && startDelta.isEmpty && channelsToPlay.exists(chns => chns.contains(channel))) {
      startDelta = Some(musicItem.timeItem.start)
    }
  }

  def updateStartTime(musicItem: MusicItem) =
    musicItem.timeItem.start = musicItem.timeItem.start - startDelta.getOrElse(0f)

  def updateStartTime(grain: AbsoluteGrain) =
    grain.start = grain.start - startDelta.getOrElse(0f)

  def shouldPlayItem(channel: Int): Boolean = {
    channelsToPlay.isEmpty || channelsToPlay.exists(chns => chns.contains(channel))
  }

  def playMusicItem(musicItem: MusicItem, channel: Int, playGrains: Boolean) = {

    val baseSourceArgs =
      layers.map(ls =>
        BaseArgument(targetNodeId = ls.getGroup(channel, GroupName.SOURCE)).arguments).
        getOrElse(defaultBase.arguments)


    val outputArgs = if(playGrains) OutbusArgument(16 + (channel * 2)).arguments else Seq()

    val startTime = musicItem.timeItem.start

    //println(s"Item start $startTime channel: $channel item: $musicItem")
    println(s"Item start $startTime channel: $channel bus: ${16 + (channel * 2)}")
    player.sendNew(
      musicItem.chord.instrument.arguments ++
        baseSourceArgs ++
        outputArgs ++
        TimeArgument(musicItem.timeItem.duration, musicItem.gesture.attackTime).arguments ++
        PanArgument(musicItem.position.start, musicItem.position.end).arguments ++
        AmpArgument(musicItem.gesture.amplitude).arguments ++
        AttackArgument3(Left(musicItem.gesture.attackCurve.name.name), Left(musicItem.gesture.decayCurve.name.name)).arguments ++
        makeFrequenciesArgument(musicItem.chord).arguments ++
        makeBwsArgument(musicItem.chord).arguments, absoluteTimeToMillis(startTime))
  }

  val noiseGrain = NoiseGrain2

  def playGrain(grain: AbsoluteGrain, channel: Int) = {
    val patternInput = InbusArgument(16 + (channel * 2))
    val patternOutput = OutbusArgument(0)
    val baseGrainArgs = BaseArgument(targetNodeId = layers.get.getGroup(channel, GroupName.GRAIN)).arguments

    val startTime = grain.start

    player.sendNew(
      noiseGrain.arguments ++
      baseGrainArgs ++
      patternInput.arguments ++
      patternOutput.arguments ++
      AttackArgument2(Left(grain.curve.name.toString())).arguments ++
      TimeArgument(grain.duration, grain.attackTime).arguments ++
      AmpArgument(grain.amplitude).arguments,
    absoluteTimeToMillis(startTime)
    )
  }
}