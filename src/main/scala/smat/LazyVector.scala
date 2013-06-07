package smat
import scala.reflect.ClassTag

class LazyVector[A:ClassTag](val length: Int,
		    val f: Int => A,
		    val isRow: Boolean = false) 
  extends Vector[A]{

  lazy val data: Array[A] = Array.tabulate(length)(f)
  
  def apply(i:Int, j:Int) = {
    if(isRow){
      data(j)
    } else {
      data(i)
    }
  }

  override def apply(i:Int) = data(i)

  def t = new LazyVector[A](length, f, !isRow) 
}

