package smat

import collection.mutable

class LazyMatrix[A](val rows:Int, val cols:Int, val func:(Int,Int)=>A) extends Matrix[A]{
  
  var cache = mutable.Map[(Int,Int),A]()

  override def apply(r:Int, c:Int)={
    def add:A={
      val res=func(r,c)
      cache+=((r,c) -> res)
      res
    }
    cache.getOrElse((r,c),add)
  }
}
