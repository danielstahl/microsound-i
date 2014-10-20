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
            println(s"playing $musicItem at channel $channel")
            updateStartTime(musicItem, channel)
            val startTime = getStartTime(musicItem)
            playMusicItem(musicItem, channel, startTime, playGrains)
            if(playGrains)
              musicItem.grains.foreach {
                grain => playGrain(grain, channel, startTime)
              }

          }
      }
  }

  var startDelta: Option[Float] = None

  def updateStartTime(musicItem: MusicItem, channel: Int): Unit = {
    if(channelsToPlay.isDefined && startDelta.isEmpty && channelsToPlay.exists(chns => chns.contains(channel))) {
      startDelta = Some(musicItem.timeItem.start)
    }
  }

  def getStartTime(musicItem: MusicItem): Float =
    musicItem.timeItem.start - startDelta.getOrElse(0f)

  def getStartTime(grain: AbsoluteGrain): Float =
    grain.start - startDelta.getOrElse(0f)

  def shouldPlayItem(channel: Int): Boolean = {
    channelsToPlay.isEmpty || channelsToPlay.exists(chns => chns.contains(channel))
  }

  def playMusicItem(musicItem: MusicItem, channel: Int, startTime: Float, playGrains: Boolean) = {
    val baseOutput = OutbusArgument(16 + (channel * 2)).arguments

    val baseSourceArgs =
      layers.map(ls =>
        BaseArgument(targetNodeId = ls.getGroup(channel, GroupName.SOURCE)).arguments).
        getOrElse(defaultBase.arguments)

    val outputArgs = if(playGrains) OutbusArgument(16 + (channel * 2)).arguments else Seq()

    player.sendNew(
      musicItem.chord.instrument.arguments ++
        //defaultBase.arguments ++
        baseSourceArgs ++
        outputArgs ++
        TimeArgument(musicItem.timeItem.duration, musicItem.gesture.attackTime).arguments ++
        PanArgument(musicItem.position.start, musicItem.position.end).arguments ++
        AmpArgument(musicItem.gesture.amplitude).arguments ++
        AttackArgument3(Left(musicItem.gesture.attackCurve.name.name), Left(musicItem.gesture.decayCurve.name.name)).arguments ++
        makeFrequenciesArgument(musicItem.chord).arguments ++
        makeBwsArgument(musicItem.chord).arguments, absoluteTimeToMillis(startTime))
  }

  /*
  player.sendNew(noiseGrain.arguments ++
                  baseGrainArgs ++
                  patternInput ++
                  patternOutput ++
                  AttackArgument2(attackType).arguments ++
                  TimeArgument(pdur, attack).arguments ++
                  AmpArgument(amp).arguments,
                  (pstart * 1000).round.toLong)
  * */

  val noiseGrain = NoiseGrain2

  def playGrain(grain: AbsoluteGrain, channel: Int, startTime: Float) = {
    val patternInput = InbusArgument(16 + (channel * 2))
    val patternOutput = OutbusArgument(0)
    val baseGrainArgs = BaseArgument(targetNodeId = layers.get.getGroup(channel, GroupName.GRAIN)).arguments

    player.sendNew(
      noiseGrain.arguments ++
      //defaultBase.arguments ++
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



/*
def internalPlay2(absoluteTime: Float, track: Int)(implicit player: MusicPlayer, layers: Layers) = {
    val baseOutput = OutbusArgument(16 + (track * 2)).arguments
    val patternInput = InbusArgument(16 + (track * 2)).arguments
    val patternOutput = OutbusArgument(0).arguments

    val baseSourceArgs = BaseArgument(targetNodeId = layers.getGroup(track, SOURCE)).arguments
    val baseGrainArgs = BaseArgument(targetNodeId = layers.getGroup(track, GRAIN)).arguments
    val baseAmp = AmpArgument(0.9f).arguments

    var tempTime = absoluteTime
    val noiseGrain = NoiseGrain2
    ((bases, notes, pans).zipped.toSeq, pulseParts).zipped.foreach {
      case ((baseValue, noteValue, panValue), pulsePart) =>
        player.sendNew(noteValue.instrument.arguments ++
          baseSourceArgs ++
          baseValue.time.arguments ++
          baseAmp ++
          panValue.arguments ++
          baseOutput ++
          noteValue.args.flatMap(_.arguments), (tempTime * 1000).round.toLong)

        pulsePart.scaleAndSum(tempTime, baseValue.time.dur).foreach {
          case (start, dur, pattern) => {
            pattern.scaleAndSum(start, dur).foreach {
              case (pstart, pdur, amp, (attackType, attack)) => {
                player.sendNew(noiseGrain.arguments ++
                  baseGrainArgs ++
                  patternInput ++
                  patternOutput ++
                  AttackArgument2(attackType).arguments ++
                  TimeArgument(pdur, attack).arguments ++
                  AmpArgument(amp).arguments,
                  (pstart * 1000).round.toLong)
              }
            }
          }
        }
        tempTime = tempTime + baseValue.delta
    }

  }
* */