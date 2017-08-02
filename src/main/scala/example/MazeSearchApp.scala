package example

/**
  * Created by hinoue on 2017/08/02.
  */
object MazeSearchApp extends App {

  import rescala._
  import workflow._
  import Workflows._
  import scalaz.std.list._
  import scalaz.syntax.foldable._

  //type Node = (Int, Int)

  case class Node(point:(Int,Int), var visited:Boolean = false)

  //wall
  val LLLLLL = (Int.MinValue, Int.MinValue)

  val maze0:Set[Node] = (Set(
    (0, 0), (1, 0), (2, 0), (3, 0),
    (0, 1), LLLLLL, (2, 1), LLLLLL,
    (0, 2), LLLLLL, (2, 2), LLLLLL,
    (0, 3), (1, 3), (2, 3), (3, 3)
  ) - LLLLLL).map(p => Node(p))

  val maze1:Set[Node] = (Set(
    (0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0),
    (0, 1), LLLLLL, (2, 1), LLLLLL, LLLLLL, (5, 1),
    (0, 2), LLLLLL, (2, 2), LLLLLL, (4, 2), LLLLLL,
    (0, 3), (1, 3), (2, 3), (3, 3), (4, 3), (5, 3),
    LLLLLL, (1, 4), LLLLLL, LLLLLL, LLLLLL, (5, 4),
    (0, 5), (1, 5), LLLLLL, (3, 5), (4, 5), (5, 5)
  ) - LLLLLL).map(p => Node(p))

  def isVisited(n: Node): Boolean = {
    n.visited
  }

  def visited(n: Node): Unit = {
    n.visited = true
  }

  def unknown(n: Node): Unit = {
    n.visited = false
  }

  def neighbors(n: Node, maze: Set[Node]): List[Node] = {
    val candidates = List((1, 0), (0, 1), (-1, 0), (0, -1)).map(p =>
      (n.point._1 + p._1, n.point._2 + p._2)
    )
    maze.filter(n => candidates.exists(_ == n.point)).toList
  }

  // imitate timer
  val moveCount = Var(0)

  def move(n: Node, info: String = "") = {
    moveCount() = moveCount.now + 1
    println(info + "[move to " + n.point + "]")
  }

  def moveFromTo(from:Node, to:Node):Workflow[Unit] =
    move(to) %% (_ => move(from, "comp:"))

  def visit(n: Node, maze: Set[Node]): Workflow[Unit] = {
    for {
      _ <- visited(n) %% (_ => unknown(n))
      _ <- neighbors(n, maze).foldLeftM[Workflow, Unit](()) { (_, neighbor) =>
        if (!isVisited(neighbor)) {
          sub {
            for {
              _ <- moveFromTo(n,neighbor)
              _ <- visit(neighbor, maze)
              _ <- move(n, "back:")
            } yield ()
          }
        }
        else catom(())()
      }

    } yield ()
  }

  def timeout(threshold: Int, ctx:Context = Abort) = {
    val now = moveCount.now
    Signal {
      if (moveCount() > now + threshold) ctx else Continue
    }
  }

  def search(maze: Set[Node], max: Int) = {
    val startNode: Node = maze.find(_.point == (0,0)).orElse(Some(maze.head)).get
    println("start from:" + startNode)
    run(visit(startNode, maze), () => timeout(max,Abort))
    val visitedNodes = maze.filter(_.visited).map(_.point)
    println("visited:" + visitedNodes)
  }

  search(maze0, 10)
  search(maze0, 10)
  search(maze0, 10)

  search(maze1, 20)
  search(maze1, 20)
  search(maze1, 20)


  // reset maze
  List(maze0,maze1).foreach(_.foreach(_.visited = false))
  ////// test for Restart

  def searchRestart(maze: Set[Node], max: Int) = {
    val startNode: Node = maze.find(_.point == (0,0)).orElse(Some(maze.head)).get
    println("start from:" + startNode)
    run(visit(startNode, maze), () => timeout(max,Restart))
    val visitedNodes = maze.filter(_.visited).map(_.point)
    println("visited:" + visitedNodes)
  }

  println("**************\n search with restart \n**************")
  searchRestart(maze0, 10)
  assert(maze0.forall(_.visited))

}
