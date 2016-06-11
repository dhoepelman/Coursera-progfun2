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
  
  @tailrec
  private def dumpHeap(h:H, acc:List[Int] = Nil) : List[Int] = if(isEmpty(h)) acc else dumpHeap(deleteMin(h), findMin(h) :: acc)

  property("intlist") = forAll { (ns: List[Int]) =>
    val h = ns.foldLeft(empty)((h,n) => insert(n,h))
    (ns.sortWith(_ > _), dumpHeap(h)).zipped.forall(_ == _)
  }

}
