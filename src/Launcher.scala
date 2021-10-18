import java.awt.{Canvas, Color, Dimension}
import javax.swing.{JFrame, WindowConstants}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Launcher extends App {

  val display = new Display
  val rule = Rule(124)
  val initial = List.fill(255)(Random.nextBoolean)
  var last = initial
  while(true) {
    display.render(last)
    last = generate(last, rule)
  }

  def Rule(i: Int): List[Boolean] = {
    val binary = i.toBinaryString
    val fullString = List.fill(8-binary.length)("0").mkString.appendedAll(binary)
    fullString.toList.map(c => {
      if(c == '0') false
      else true
    })
  }

  def apply(tuple: (Boolean, Boolean, Boolean), rule: List[Boolean]): Boolean = {
    tuple match {
      case (false, false, false) => rule(7)
      case (false, false, true) => rule(6)
      case (false, true, false) => rule(5)
      case (false, true, true) => rule(4)
      case (true, false, false) => rule(3)
      case (true, false, true) => rule(2)
      case (true, true, false) => rule(1)
      case (true, true, true) => rule(0)
    }
  }

  def generate(row1: List[Boolean], rule: List[Boolean]): List[Boolean] = {
    val output = new ListBuffer[Boolean]()
    val row = row1.toArray
    for(i <- row.indices) {
      var tuple = (false, false, false)
      if(i == 0) tuple = (row.last, row.head, row(i + 1))
      else if(i == row.length - 1) tuple = (row(i - 1), row.last, row.head)
      else tuple = (row(i - 1), row(i), row(i + 1))
      val result = apply(tuple, rule)
      output.addOne(result)
    }
    output.toList
  }

  def show(row: List[Boolean]): Unit = {
    for(bool <- row) {
      if(bool) print("#")
      else print(" ")
    }
    println("")
  }
}
class Display extends Thread {
  private val frame = new Frame()
  def render(row: List[Boolean]): Unit = {
    frame.render(row)
  }
  class Frame extends JFrame {
    private val imageBuffer = new mutable.Queue[List[Boolean]]()
    private val capacity = 200
    val DEFAULT_WINDOW_WIDTH = 800
    val DEFAULT_WINDOW_HEIGHT = 800

    private val canvas: Canvas = new Canvas()

    setTitle("Cellular Automata")
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setResizable(false)
    canvas.setPreferredSize(new Dimension(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT))
    canvas.setFocusable(false)
    add(canvas)
    pack()
    canvas.createBufferStrategy(2)

    setLocationRelativeTo(null)
    setVisible(true)

    def render(row: List[Boolean]): Unit = {
      imageBuffer.enqueue(row)
      if(imageBuffer.size == capacity) imageBuffer.dequeue()
      val bufferStrategy = canvas.getBufferStrategy
      val graphics = bufferStrategy.getDrawGraphics

      for(i <- imageBuffer.indices) {
        val list = imageBuffer(i).toArray
        for(j <- list.indices) {
          val bool = list(j)
          if(bool) {
            graphics.setColor(Color.BLACK)
            graphics.fillRect(j*4, i*4, 4, 4)
          }
          else {
            graphics.setColor(Color.WHITE)
            graphics.fillRect(j*4, i*4, 4, 4)
          }
        }
      }

      graphics.dispose()
      bufferStrategy.show()
    }

  }

}
