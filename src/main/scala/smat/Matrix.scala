package smat
import scala.reflect.ClassTag
import smat.math._

/**
 * 2D martix dependency injection trait
 * defines matrix interface and provides fallback ops 
 */
trait Matrix[A] { 
  def apply(row:Int, col:Int):A
  
  def rows: Int
  def cols: Int

  def +(that: Matrix[A])(implicit s:Semiring[A], tag:ClassTag[A]):Matrix[A]={
    if(that.rows!= this.rows || that.cols != this.cols)
      throw new MatrixSizeException
    new LazyMatrix[A](rows,cols,(r,c)=>s.add(this(r,c), that(r,c)))
  }
  
  def -(that: Matrix[A])(implicit ev:Ring[A], tag:ClassTag[A]):Matrix[A]={
    if(that.rows!= this.rows || that.cols != this.cols)
      throw new MatrixSizeException
    new LazyMatrix[A](rows,cols,(r,c)=>ev.sub(this(r,c), that(r,c)))
  }

  def *(that: Matrix[A])(implicit s:Semiring[A], tag:ClassTag[A]):Matrix[A]={
     if(that.rows!= this.cols)
      throw new MatrixSizeException
    def mm(r:Int,c:Int):A={
      var i:Int=0
      var res:A=s.zero
      while(c<this.cols){
	val prod=s.mul(this(r,i),that(i,c))
	res=s.add(res,prod)
	i+=1
      }
      res
    }
    new LazyMatrix[A](rows,cols,mm)
  }
  
  def *(that: A)(implicit s:Semiring[A], tag:ClassTag[A]):Matrix[A]={
    new LazyMatrix[A](rows,cols,(r,c)=>s.mul(this(r,c),that))
  }

  def t(implicit tag:ClassTag[A]):Matrix[A]={    
    new LazyMatrix[A](rows,cols,(r,c)=>this(c,r))
  }
}

class MatrixSizeException extends Exception

object Matrix{
  
}
