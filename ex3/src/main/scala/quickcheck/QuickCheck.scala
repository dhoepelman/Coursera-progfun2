package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Inserting a smaller element into a heap gets the smallest back
//  property("gen1") = forAll { (h: H) =>
//    val m = hmin(h)
//    findMin(insert(m, h)) == m
//  }
//
//  private def hmin(h:H) = if (isEmpty(h)) 0 else findMin(h)
//
//  property("twoInEmpty") = forAll { (h: H) =>
//    val m = hmin(h)
//    (m > Int.MinValue) ==> {
//      val m1 = m - 1
//      findMin(insert(m, insert(m1, h))) == m1 &&
//      findMin(insert(m1, insert(m, h))) == m1
//    }
//  }
//
//  property("delMin") = forAll { (n : Int) =>
//    val h = insert(n, empty)
//    isEmpty(deleteMin(h))
//  }
//
  @tailrec
  private def dumpHeap(h:H, acc:List[Int] = Nil) : List[Int] = if(isEmpty(h)) acc else dumpHeap(deleteMin(h), findMin(h) :: acc)
//
//  property("sorted") = forAll { (h:H) =>
//    val seq = dumpHeap(h)
//    val sorted = seq.sortWith(_ > _)
////    System.out.println(seq.toString)
////    System.out.println(sorted.toString)
//    seq == sorted
//  }
//
//  property("sorted2") = forAll { (h:H) =>
//    val x = dumpHeap(h)
//    (x, x.tail).zipped.forall(_ >= _)
//  }
//
//  property("min-meld") = forAll { (h1:H, h2:H) =>
//    val m1 = findMin(h1)
//    val m2 = findMin(h2)
//    val mmeld = findMin(meld(h1,h2))
//    (mmeld == m1 && m1 <= m2) || (mmeld == m2 && m2 < m1)
//  }

  property("intlist") = forAll { (ns: List[Int]) =>
    val h = ns.foldLeft(empty)((h,n) => insert(n,h))
    (ns.sortWith(_ > _), dumpHeap(h)).zipped.forall(_ == _)
  }

}
