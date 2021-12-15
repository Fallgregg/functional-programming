package scalashop

import scala.math.pow

object IndividualTask extends App{

  def toList(range: Seq[Int], n: Int): List[Double] =
    range.map(x => (x, n)).collect(individualTask).toList

  def individualTask: PartialFunction[(Int, Int), Double] = {
    case (x, n) if x < n => pow(x, n)
    case (x, n) if x > n => n
  }

  def get_result(range: Seq[Int], n: Int, numTasks: Int): List[Double] = {
    val elemsPerTask: Int = range.length / numTasks max 1
    val startPoints = 0 to range.length by elemsPerTask

    val tasks = startPoints.map(t => {
      task {
        toList(range.slice(t, t + elemsPerTask), n)
      }
    })

    var result = List[Double]()
    tasks.map(t => t.join()).foreach(v => {
      result = result ::: v
    })
    result
  }

  val n = 3
  val y = toList(-250 to 250, n)
  println(y + "\n")

  val result = get_result(-250 to 250, n, 3)
  println(result)
}