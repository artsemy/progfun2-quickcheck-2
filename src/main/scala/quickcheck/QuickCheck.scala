package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      elem <- arbitrary[A]
      heap <- genHeap
    } yield insert(elem, heap)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("add and compare two elems") = forAll { (x1: A, x2: A) =>
    (x1 min x2) == findMin(insert(x1, insert(x2, empty)))
  }

  property("insert and remove") = forAll { (x: A) =>
    isEmpty(deleteMin(insert(x, empty)))
  }

  property("sorted by deleting min") = forAll { (h: H) =>
    @tailrec
    def loop(heap: H, acc: Seq[Int]): Seq[Int] =
      if isEmpty(heap) then
        acc
      else
        loop(deleteMin(heap), acc :+ findMin(heap))

    val maybeSorted = loop(h, Seq())
    maybeSorted == maybeSorted.sorted
  }

  property("min of melding") = forAll { (h1: H, h2: H) =>
    val m1 = (isEmpty(h1), isEmpty(h2)) match
      case (true, true) => 0
      case (true, false) => findMin(h2)
      case (false, true) => findMin(h1)
      case (false, false) => findMin(h1) min findMin(h2)
    val melded = meld(h1, h2)
    val m2 = if isEmpty(melded) then 0 else findMin(melded)
    m1 == m2
  }

  property("insert 3 into empty and find max by deleting 2") = forAll { (x1: A, x2: A, x3: A) =>
    val heap = insert(x1, insert(x2, insert(x3, empty)))
    findMin(deleteMin(deleteMin(heap))) == (x1 max (x2 max x3))
  }

