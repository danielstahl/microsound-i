package music

import java.awt.{Font, RenderingHints}
import javafx.embed.swing.JFXPanel

import scala.swing._
import scala.swing.event.Event
import scalafx.application.Platform
import scalafx.geometry.Insets
import scalafx.scene.canvas._
import scalafx.scene.layout.BorderPane
import scalafx.scene.{Scene, control}
import scalafx.stage.Stage



case class DataPlotterFx() {
  new JFXPanel()

  var gc: GraphicsContext = null

  def show(): Unit = {
    Platform.runLater {
      // Create dialog using `Stage` (not `PrimaryStage`)
      val dialogStage = new Stage {
        minHeight = 700
        minWidth = 1000
        scene = new Scene {
          root = new BorderPane {
            padding = Insets(25)
            center = new control.ScrollPane {
              content = new Canvas(10000, 2000) {
                gc = graphicsContext2D
                gc.font = scalafx.scene.text.Font(9)
                gc.setLineWidth(.2)
              }
            }
            bottom = new control.Label("Data plotter")
          }
        }
      }
      // Display the dialog
      dialogStage.show
    }
  }

  def plot(func: (GraphicsContext) => Unit): Unit = {
    Platform.runLater {
      func(gc)
    }
  }
}

case class TimeItemPlotterActor(dataPlotter: DataPlotterFx) extends LeafActor {
  var channel = 0
  def receive = {
    case event @ TimeItemsEvent(timeItems) =>
      event.channel = channel
      channel = channel + 1
      timeItems.foreach {
        timeItem =>
          dataPlotter.plot {
            gc =>
              val ypos = 20d + (event.channel * 20d)
              val startPos = xpos(timeItem.start)
              val durationEndPos = xpos(timeItem.start + timeItem.duration)
              gc.strokeLine(startPos.toDouble, ypos, durationEndPos.toDouble, ypos)
              gc.strokeLine(startPos.toDouble, ypos, startPos.toDouble, ypos - 10)
              val timeString = f"${timeItem.start}%.2f"
              gc.strokeText(timeString, startPos.toDouble + 5, ypos - 5)

          }
      }
  }

  def xpos(xtime: Float): Double = 10 + (xtime * 10).round
}











class PlottableComponent extends Component {	
	var plottable: Option[Plottable] = None
	
	override def paintComponent(g: Graphics2D) {
	  plottable match {
	    case p =>
	      g.setFont(new Font("Arial", Font.PLAIN, 9))
	      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
	      p.get.plot(g)
	  }	  
	}
	
	reactions += {
	  case e: PlottableEvent =>
	    plottable = Option(e.plottable)
	}
}

class PlotterFrame(gc: java.awt.GraphicsConfiguration = null) extends Frame(gc) {
  override def closeOperation() {
    this.visible = false
  }
}

case class DataPlotter() extends Reactor {
	var top: PlotterFrame = setupGui
	val publisher: Publisher = PlottablePublisher()
	
	def setupGui: PlotterFrame = {
		Swing.onEDT {
			top = new PlotterFrame {
			  title = "Dataplotter"
			  contents = new BorderPanel {				  
				  add(makeScrollPane(makeComponent(publisher)), BorderPanel.Position.Center)
			  }	
			  if(size == new Dimension(0,0)) {
			    pack()
			  }
			}
		}
	  top
	}
  
	def plot(plottable: Plottable) {	  
		Swing.onEDT {			
			top.visible = true
			publisher.publish(PlottableEvent(plottable))
		}
	}

  def plot(plotter: (Graphics2D) => Unit) {
    plot(
      new Plottable {
        override def plot(g: Graphics2D): Unit = plotter(g)
      }
    )
  }

	def makeScrollPane(component: Component): ScrollPane = {
	  new ScrollPane(component) {
	    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Always
	  }
	}
	
	def makeComponent(publisher: Publisher): PlottableComponent = {
	  new PlottableComponent {
	    preferredSize = new Dimension(7000, 1000)
	    listenTo(publisher)
	  }
	}
}

case class PlottableEvent(plottable: Plottable) extends Event

case class PlottablePublisher() extends Publisher


trait Plottable {
	def plot(g: Graphics2D)
}

