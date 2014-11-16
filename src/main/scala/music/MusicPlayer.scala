package music

import java.net.InetAddress
import java.util.Date
import java.util.concurrent.PriorityBlockingQueue

import com.illposed.osc.{OSCBundle, OSCMessage, OSCPacket, OSCPortOut}

case class MusicPlayer() {
  val sender: OSCPortOut = new OSCPortOut(InetAddress.getLocalHost, 57110)

  val clock: PlayerClock = PlayerClock()

  def play(playable: Playable): Unit = {
    clock.reset()
    playable.play()(this)
  }

  def play(playable: (MusicPlayer) => Unit): Unit = {
    play(
      new Playable {
        override def play()(implicit player: MusicPlayer): Unit = playable(player)
      }
    )
  }

  def startPlay(): Unit = {
    clock.reset()
    playThread.start()
  }

  def sendNew(arguments: Seq[Object], deltaTime: Long) = {
    //println(s"Sending message at $deltaTime with args $arguments")
    val theMessages: Array[Object] = arguments.toArray
    sendBundle(Array(new OSCMessage("/s_new", theMessages)), deltaTime)
  }

  def makeNew(arguments: Seq[Object]) = {
    val theMessages: Array[Object] = arguments.toArray
    new OSCMessage("/s_new", theMessages)
  }

  /**
   * Add node to head of group
   */

  def makeGroupHead(groupId: Integer, nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId, new Integer(0), groupId)
    println(s"making /g_new with arguments ${theArguments.toList}")
    new OSCMessage("/g_new", theArguments)
  }

  /**
   * Add node to tail of group
   */

  def makeGroupTail(groupId: Integer, nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId, new Integer(1), groupId)
    println(s"making /g_new with arguments ${theArguments.toList}")
    new OSCMessage("/g_new", theArguments)
  }

  def makeAddHeadNode(groupId: Integer, nodeId: Integer) = {
    val theArguments: Array[Object] = Array(groupId, nodeId)
    println(s"making /g_head with arguments ${theArguments.toList}")
    new OSCMessage("/g_head", theArguments)
  }

  def makeAddTailNode(groupId: Integer, nodeId: Integer) = {
    val theArguments: Array[Object] = Array(groupId, nodeId)
    println(s"making /g_tail with arguments ${theArguments.toList}")
    new OSCMessage("/g_tail", theArguments)
  }

  def makeFreeAll(nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId)
    new OSCMessage("/g_freeAll", theArguments)
  }

  def makeAllocBuffer(bufferNumber: Integer, numberOfFrames: Integer, numberOfChannels: Integer = 2) = {
    val theArguments: Array[Object] = Array(bufferNumber, numberOfFrames, numberOfChannels)
    println(s"making /b_alloc with arguments ${theArguments.toList}")
    new OSCMessage("/b_alloc", theArguments)
  }

  /**
   *
   * @param bufferNumber
   * @param pathName
   * @param headerFormat one of "aiff", "next", "wav", "ircam"", "raw"
   * @param sampleFormat one of "int8", "int16", "int24", "int32", "float", "double", "mulaw", "alaw"
   * @param numberOfFrames  If number of frames is less than zero, all samples from the starting frame to the end of the buffer are written
   * @param startingFrameInBuffer default 0
   * @param leaveFileOpen  If opening a file to be used by DiskOut ugen then you will want to set "leave file open" to one, otherwise set it to zero. If "leave file open" is set to one then the file is created, but no frames are written until the DiskOut ugen does so.
   * @return
   */
  def makeWriteSoundFile(bufferNumber: Integer, pathName: String, headerFormat: String = "aiff", sampleFormat: String = "float",
                         numberOfFrames: Integer = -1, startingFrameInBuffer: Integer = 0, leaveFileOpen: Integer = 1) = {
    val theArguments: Array[Object] = Array(bufferNumber, pathName, headerFormat, sampleFormat, numberOfFrames, startingFrameInBuffer, leaveFileOpen)
    println(s"making /b_write with arguments ${theArguments.toList}")
    new OSCMessage("/b_write", theArguments)
  }

  def makeFreeBuffer(bufferNumber: Integer) = {
    val theArguments: Array[Object] = Array(bufferNumber)
    println(s"making /b_free with arguments ${theArguments.toList}")
    new OSCMessage("/b_free", theArguments)
  }

  case class ComparableBundle(bundle: OSCBundle) extends Comparable[ComparableBundle] {
    override def compareTo(o: ComparableBundle): Int = {
      bundle.getTimestamp.compareTo(o.bundle.getTimestamp)
    }
  }

  val playQueue: PriorityBlockingQueue[ComparableBundle] = new PriorityBlockingQueue()

  val playThread = new Thread(new Runnable {
    val BUFFER_TIME = 1000 * 30

    def peekTime: Option[Date] = {
      Option(playQueue.peek).map {
        bundle => bundle.bundle.getTimestamp
      }
    }

    def peekOverdue: Boolean = {
      peekTime.exists {
        time => (time.getTime - BUFFER_TIME) < System.currentTimeMillis()
      }
    }

    override def run() = {
      println(s"Starting background player")
      while (true) {
        while (peekOverdue) {
          sender.send(playQueue.poll().bundle)
        }
        Thread.sleep(1000)
      }
    }
  })

  def sendBundle(messages: Array[OSCPacket], deltaTime: Long) = {
    val theTime = new Date(clock.start + deltaTime)
    playQueue.offer(ComparableBundle(new OSCBundle(messages, theTime)))
  }
}

case class PlayerClock() {
  val DELAY: Long = 2000
  var start: Long = System.currentTimeMillis() + DELAY

  def reset() = start = System.currentTimeMillis() + DELAY
}

trait Playable {
  def play()(implicit player: MusicPlayer)
}