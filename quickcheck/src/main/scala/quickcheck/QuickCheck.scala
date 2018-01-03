package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val m = a min b
    val h = insert(b, insert(a, empty))
    findMin(h) == m
  }

  property("min3") = forAll{ (_h1: H, _h2: H, v1: Int, v2: Int) =>

    val h1 = if (isEmpty(_h1)) insert(v1, _h1) else _h1
    val h2 = if (isEmpty(_h2)) insert(v2, _h2) else _h2
    val ml = meld(h1, h2)
    findMin(h1) == findMin(ml) || findMin(h2) == findMin(ml)
  }

  property("sorted") = forAll { h: H =>
    def checkSorting(h: H): List[Int] = {
      if (isEmpty(h)) List()
      else {
        val min = findMin(h)
        min :: checkSorting(deleteMin(h))
      }
    }

    val xs = checkSorting(h)
    xs == xs.sorted
  }

  property("del1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  // Take two arbitrary heaps, meld together.
  // Then remove min from 1 and insert into 2, meld the results.
  // Compare two melds by comparing sequences of ranks.
  property("meldMinMove") = forAll {(_h1: H, _h2: H, v1: Int, v2: Int) =>
    val h1 = if (isEmpty(_h1)) insert(v1, _h1) else _h1
    val h2 = if (isEmpty(_h2)) insert(v2, _h2) else _h2

    val mld1 = meld(h1, h2)
    val mv = findMin(h1)
    val m1 = deleteMin(h1)
    val m2 = insert(mv, h2)
    val mld2 = meld(m1, m2)

    def cmpRanks(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else if (findMin(h1) != findMin(h2)) false
      else cmpRanks(deleteMin(h1), deleteMin(h2))

    cmpRanks(mld1, mld2)
  }
}
