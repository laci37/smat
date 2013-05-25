package smat
import scala.reflect.ClassTag
import smat.math._

/**2D martix dependency injection trait
 * 
 * defines matrix interface and provides fallback ops 
 */
trait Matrix[A]{ 
  /**returns the element in the specified row and column
   *
   */ 
  def apply(row:Int, col:Int):A
  
  /**returns the number of rows in this matrix
   *
   */ 
  def rows: Int

  /**returns the numbor of columns in this matrix
   *
   */  
  def cols: Int

  /**adds this to that, returns the result in a new matrix
   *
   */ 
  def +(that: Matrix[A])(implicit s:Semiring[A], tag:ClassTag[A]):Matrix[A]={
    if(that.rows!= this.rows || that.cols != this.cols)
      throw new MatrixSizeException
    new LazyMatrix[A](rows,cols,(r,c)=>s.add(this(r,c), that(r,c)))
  }
  
  /**subtracts that from this, returns the result in a new matrix
   *
   */ 
  def -(that: Matrix[A])(implicit ev:Ring[A], tag:ClassTag[A]):Matrix[A]={
    if(that.rows!= this.rows || that.cols != this.cols)
      throw new MatrixSizeException
    new LazyMatrix[A](rows,cols,(r,c)=>ev.sub(this(r,c), that(r,c)))
  }

  /**multiplies this by that, returns the result in a new matrix
   *
   */ 
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
  
  /**mulitplies this by a scalar, returns the result in a new matrix.
   *
   */ 
  def *(that: A)(implicit s:Semiring[A], tag:ClassTag[A]):Matrix[A]={
    new LazyMatrix[A](rows,cols,(r,c)=>s.mul(this(r,c),that))
  }

  /**transposes this matrix returns the result in a new one
   *
   */ 
  def t(implicit tag:ClassTag[A]):Matrix[A]={    
    new LazyMatrix[A](rows,cols,(r,c)=>this(c,r))
  }

  //traversable methods
  
  def foreach(f: A => Unit) { 
    var c=0
    var r=0
    while(r<rows){ 
      while(c<cols){ 
	f(apply(r,c))
	c+=1
      }
      r+=1
    }
  }

  def map[B:ClassTag](f: A => B):Matrix[B] = { 
    new LazyMatrix[B](rows,cols,{ (r,c) => f(this.apply(r,c)) })
  }
}


class MatrixSizeException extends Exception

object Matrix{
  
}
