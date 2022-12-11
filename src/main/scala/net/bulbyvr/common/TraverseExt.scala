package net.bulbyvr.common

import cats.Traverse
import cats.implicits.*
import collection.mutable as mut
object TraverseExt {
  extension [F[_], A](t: F[A])(using traverse: Traverse[F]) {
    def splitBy(fun : A => Boolean) : Iterator[Iterator[A]] = {
      val res = mut.ListBuffer[mut.ListBuffer[A]](mut.ListBuffer())
      traverse.map(t){ (it : A) =>
        // if having fun
        if (fun(it)) {
          res += mut.ListBuffer[A]()
        } else {
          res.last += it
        }
        ()
      }
      res.map(_.iterator).iterator
    }
    def splitOn(value : A) = {
      t.splitBy(_ == value)
    }
    def union[R[_]](other : R[A])(using tr : Traverse[R]): Set[A] = {
      traverse.toList(t).toSet ++ tr.toList(other).toSet
    }
    def intersect[R[_]](other : R[A])(using tr : Traverse[R]) : Set[A] = {
      traverse.toList(t).toSet.intersect(tr.toList(other).toSet)
    }
    def difference[R[_]](other : R[A])(using tr : Traverse[R]) : Set[A] = {
      traverse.toList(t).toSet.diff(tr.toList(other).toSet)
    }
    def symdiff[R[_]](other : R[A])(using tr : Traverse[R]) : Set[A] = {
      t.union(other) diff t.intersect(other)
    }
  }
}
