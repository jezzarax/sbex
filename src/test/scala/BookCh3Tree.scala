import org.scalatest._

class BookCh3Tree extends FlatSpec with Matchers {

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def size[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right) + 1
      }
    }

    def maximum(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(v) => v
        case Branch(left, right) => maximum(left) max maximum(right)
      }
    }

    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => (depth(left) max depth(right)) + 1
      }
    }
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }

    def fold[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = {
      tree match {
        case Leaf(v) => f(v, z)
        case Branch(left, right) => fold(right, fold(left, z)(f))(f)
      }
    }
  }

  "Ex3.25" should "implement a size function that counts all the tree nodes" in {
    Tree.size(Leaf(1)) should be (1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be (3)
    Tree.size(Branch(Leaf(3), Branch(Leaf(1), Leaf(2)))) should be (5)
  }

  "Ex3.26" should "implement maximum function returning maximal value of a node in a tree" in {
    Tree.maximum(Leaf(1)) should be (1)
    Tree.maximum(Branch(Leaf(1), Leaf(2))) should be (2)
    Tree.maximum(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))) should be (5)
  }

  "Ex3.27" should "implement a depth function returning maximal depth of the tree" in {
    Tree.depth(Leaf(1)) should be (1)
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be (2)
    Tree.depth(Branch(Leaf(3), Branch(Leaf(5), Leaf(2)))) should be (3)
  }

  "Ex3.28" should "implement map for a tree" in {
    Tree.map(Leaf(1))(_+2) should be (Leaf(3))
    Tree.map(Leaf(1))(_.toString) should be (Leaf("1"))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_*2) should be (Branch(Leaf(2), Leaf(4)))
  }

  "Ex3.29" should "implement fold for a tree" in {
    Tree.fold(Leaf(1), 0)(_+_) should be (1)
    Tree.fold(Branch(Leaf(3), Leaf(2)), 1)(_*_) should be (6)
    Tree.fold(Branch(Leaf(3), Branch(Leaf(5), Leaf(2))), 1)(_*_) should be (30)
  }

  
}