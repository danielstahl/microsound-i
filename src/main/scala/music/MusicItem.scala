package music

import music.MusicActor._
import FrequencyFilter._

case class MusicItem(timeItem: TimeItem, chord: FrequencyFilterChord, position: PositionItem, gesture: GestureItem, grainLayers: List[List[AbsoluteGrain]])

case class MusicItemsEvent(musicItems: List[MusicItem]) extends MusicEvent

case class MusicItemMaker(frequencyBuilderPattern: PatternItem[FrequencyFilterChordBuilder],
                          positionItemPatterns: PatternItem[PatternItem[PositionItem]],
                          gestureItemPatterns: PatternItem[PatternItem[GestureItem]],
                          grainGestureBuilders: List[GrainGestureBuilder],
                          var listeners: MusicActorPattern = emptyActor) extends NodeActor {
  def receive = {
    case TimeItemsEvent(timeItems) =>
      val frequencyBuilder = frequencyBuilderPattern.takeItem()
      val positionItemPattern = positionItemPatterns.takeItem()
      val gestureItemPattern = gestureItemPatterns.takeItem()

      listeners.takeItem().tell(MusicItemsEvent(timeItems.map {
        timeItem =>
          val grainLayers = grainGestureBuilders.map(_.buildGrainGestures(timeItem.start, timeItem.delta, timeItem.duration))
          MusicItem(timeItem,
            frequencyBuilder.buildFrequencyChord,
            positionItemPattern.takeItem(),
            gestureItemPattern.takeItem(),
            grainLayers)
      }))
  }
}

case class MusicChannel(channel: Int, musicItems: List[MusicItem], effectItem: EffectItem)

case class MusicChannelEvent(musicChannel: MusicChannel) extends MusicEvent

case class MusicChannelMaker(var listeners: MusicActorPattern = emptyActor, effectItemPattern: PatternItem[EffectItem]) extends NodeActor {
  var channel = 0

  def receive = {
    case MusicItemsEvent(musicItems) =>
      listeners.takeItem().tell(MusicChannelEvent(MusicChannel(channel, musicItems, effectItemPattern.takeItem())))
      channel = channel + 1
  }
}

case class MusicChannelPlayer(player: MusicPlayer, nbrOfChannels: Int, channelsToPlay: Option[List[Int]] = None, playGrains: Boolean = false, playEffects: Boolean = false) extends LeafActor {
  private val defaultBase = BaseArgument()

  val layers = if (playGrains) Some(Layers(nbrOfChannels)) else None

  def playLayers(): Unit = {
    implicit val p = player
    layers.foreach {
      layers =>
        layers.play()
    }
  }


  def getMinimumTime(musicItems: List[MusicItem]): Float = {
    musicItems.map(_.timeItem.start).min
  }

  override protected def receive: PartialFunction[MusicEvent, Unit] = {
    case MusicChannelEvent(MusicChannel(channel, musicItems, effectItem)) =>
      if (playEffects) {
        playEffect(effectItem, channel, getMinimumTime(musicItems))
      }
      musicItems.foreach {
        musicItem =>
          if (shouldPlayItem(channel)) {
            updateStartDelta(musicItem, channel)
            updateStartTime(musicItem)
            playMusicItem(musicItem, channel, playGrains)
            if (playGrains)
              musicItem.grainLayers.foreach {
                grainLayer =>
                  grainLayer.foreach {
                    grain =>
                      updateStartTime(grain)
                      playGrain(grain, playEffects, channel)
                  }
              }

          }
      }
  }

  var startDelta: Option[Float] = None

  def updateStartDelta(musicItem: MusicItem, channel: Int): Unit = {
    if (channelsToPlay.isDefined && startDelta.isEmpty && channelsToPlay.exists(chns => chns.contains(channel))) {
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

  val virtualBusStart = 16
  def musicItemBus(channel: Int): Int = virtualBusStart + (channel * 4)
  def grainBus(channel: Int): Int = virtualBusStart + (channel * 4) + 2


  def playMusicItem(musicItem: MusicItem, channel: Int, playGrains: Boolean) = {

    val baseSourceArgs =
      layers.map(ls =>
        BaseArgument(targetNodeId = ls.getGroup(channel, GroupName.SOURCE)).arguments).
        getOrElse(defaultBase.arguments)

    val outputArgs = if (playGrains) OutbusArgument(musicItemBus(channel)).arguments else Seq()

    val startTime = musicItem.timeItem.start

    println(s"Item start $startTime channel: $channel bus: ${musicItemBus(channel)}")
    println(s"Grains for channel $channel will play at bus ${grainBus(channel)}")
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

  def playGrain(grain: AbsoluteGrain, playEffects: Boolean, channel: Int) = {
    val patternInput = InbusArgument(musicItemBus(channel))
    val patternOutput = if (playEffects) OutbusArgument(grainBus(channel)) else OutbusArgument(0)
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

  val effectInstrument = Reverb

  def playEffect(effectItem: EffectItem, channel: Int, startTime: Float) = {
    println(s"Effect for channel $channel will read from bus ${grainBus(channel)}")
    val effectInput = InbusArgument(grainBus(channel))
    val effectOutput = OutbusArgument(0)
    val baseEffectArgs = BaseArgument(targetNodeId = layers.get.getGroup(channel, GroupName.EFFECT)).arguments
    val reverbArgument = ReverbArgument(effectItem.mix, effectItem.room, effectItem.damp)

    player.sendNew(
      effectInstrument.arguments ++
        baseEffectArgs ++
        effectInput.arguments ++
        effectOutput.arguments ++
        reverbArgument.arguments,
      absoluteTimeToMillis(startTime)
    )
  }
}