package music

import java.util.concurrent.PriorityBlockingQueue

import com.illposed.osc.OSCPortOut
import java.net.InetAddress
import com.illposed.osc.OSCMessage
import com.illposed.osc.OSCPacket
import com.illposed.osc.OSCBundle
import java.util.Date
import com.illposed.osc

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

  def makeFreeAll(nodeId: Integer) = {
    val theArguments: Array[Object] = Array(nodeId)
    new OSCMessage("/g_freeAll", theArguments)
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
    //sender.send(new OSCBundle(messages, theTime))
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