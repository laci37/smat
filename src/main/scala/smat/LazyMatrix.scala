package smat

import collection.mutable

class LazyMatrix[A](val rows:Int, val cols:Int, val func:(Int,Int)=>A) extends Matrix[A]{
  lazy val data = Array.fill(cols){ c => Array.fill(rows){ r => func(r,c) } } 

  override def apply(r:Int, c:Int)={
    data(c)(r)
  }
}
