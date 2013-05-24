package smat
import scala.reflect.ClassTag


class LazyMatrix[A:ClassTag](val rows:Int, val cols:Int, val func:(Int,Int)=>A) extends Matrix[A]{
  lazy val data = Array.tabulate(cols,rows){ (r,c) => func(r,c) } 

  override def apply(r:Int, c:Int)={
    data(c)(r)
  }
}
