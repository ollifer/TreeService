import org.scalacheck._
import org.scalatest.{FlatSpec, Matchers}

class TreeServiceSpec extends FlatSpec with Matchers {

  "Tree with no leaves and no child nodes" should "be updated properly" in {

    val w = 20
    val emptyTree = Node(Nil, Nil)
    val actual = TreeService.updateTree(emptyTree, w)

    actual shouldEqual emptyTree
  }

  "Tree with complex structure" should " be updated properly" in {
    val tree = Node(List(200, 2, 3, 5),
      List(
        Node(List(4, 3, 14),
          List(
            Node(List(6),
              Nil
            )
          )
        ),
        Node(List(6),
          List(
            Node(List(90, 9),
              Nil
            )
          )
        )
      )
    )

    val w = 20

    val actual = TreeService.updateTree(tree, w)

    val expected = Node(List(2, 3, 5),
      List(
        Node(List(3, 4),
          List(
            Node(List(6, 14), Nil))
        ),
        Node(List(6),
          List(
            Node(List(9),
              Nil)
          )
        )
      )
    )

    actual shouldEqual expected

  }

  "Tree with complex structure" should " hold its properties when updated" in {

    val leavesGen: Gen[List[Int]] = Gen.listOfN(30, Gen.choose(0, 100))

    def nodesListGen: Gen[List[Node]] = Gen.sized { size =>
      Gen.resize(size / 3, Gen.listOfN(size, Gen.oneOf(genNode, genNode)))
    }

    def genNode: Gen[Node] = for {
      leaves: List[Int] <- leavesGen
      nodes: List[Node] <- nodesListGen
    } yield Node(leaves, nodes)


    val w = 100

    Prop.forAll(genNode) { node =>
      val updated = TreeService.updateTree(node, w)
      (updated.leaves == updated.leaves.sorted) && (updated.leaves.sum <= w)
    }.check
  }

}
