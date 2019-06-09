import scala.annotation.tailrec

object TreeService {

  def updateTree(node: Node, w: Int): Node = {

    val sorted = sort(node.leaves)

    val (leavesToUpdate, leavesToMove) = splitByW(sorted, w, 0, (Nil, sorted))

    val nodesToUpdate = if (leavesToMove.nonEmpty) {
      node.nodes match {
        case Nil => Nil
        case first :: tail => first.copy(first.leaves ::: leavesToMove) :: tail
      }
    } else {
      node.nodes
    }

    val recursivelyUpdated = nodesToUpdate.map(updateTree(_, w))
    node.copy(leavesToUpdate, recursivelyUpdated)
  }

  // split sorted list into two parts, the sum of the first part is less than or equal to the w coefficient,
  // and the sum of the second is greater
  @tailrec
  private def splitByW(sorted: List[Int], w: Int, sum: Int, acc: (List[Int], List[Int])): (List[Int], List[Int]) = sorted match {
    case Nil => (acc._1.reverse, acc._2)
    case l :: _ if l + sum > w => (acc._1.reverse, acc._2)
    case l :: tail => splitByW(tail, w, sum + l, (l :: acc._1, tail))
  }

  //insertion sort
  private def sort(input: List[Int]): List[Int] = {

    def insert(elem: Int, sorted: List[Int]): List[Int] = sorted match {
      case Nil => elem :: Nil
      case x :: tail => if (elem <= x) {
        elem :: sorted
      } else {
        x :: insert(elem, tail)
      }
    }

    input match {
      case Nil => Nil
      case x :: tail => insert(x, sort(tail))
    }

  }

}

case class Node(leaves: List[Int], nodes: List[Node])
