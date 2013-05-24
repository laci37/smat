package smat.colt

import smat._
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.DoubleFactory2D
import cern.colt.matrix.linalg.Algebra
import cern.jet.math.Functions

/** Wrapper for cern.colt.matrix.DoubleMatrix2D
 *
 *  This class is the default implementation for DMatrix
 */ 
class DoubleMatrix(val under:DoubleMatrix2D, transpose:Boolean = false) extends DMatrix{

  def rows=if(transpose) under.columns else under.rows
  def cols=if(transpose) under.rows else under.columns
  
  def apply(r:Int,c:Int)=under.get(r,c)

  def +(that:DMatrix):DMatrix = that match {
    case m:DoubleMatrix=>{
      new DoubleMatrix(under.copy().assign(m.under,Functions.plus))
    }
    case _ =>{
      DoubleMatrix.fromGeneral(super.+(that))
    }
  }

  def -(that:DMatrix):DMatrix = that match {
    case m:DoubleMatrix=>{
      new DoubleMatrix(under.copy().assign(m.under,Functions.minus))
    }
    case _ =>{
      DoubleMatrix.fromGeneral(super.-(that))
    }
  }
  

  def *(that:DMatrix):DMatrix = that match {
    case m:DoubleMatrix=>{
      new DoubleMatrix(Linalg.mult(this.under,m.under))
    }
    case _ =>{
      DoubleMatrix.fromGeneral(super.*(that))
    }
  }
  

  def *(that:Double):DMatrix = {
    new DoubleMatrix(under.copy().assign(Functions.mult(that)))
  }

  def t:DMatrix= new DoubleMatrix(under,!transpose)
} 

/** Factory for DoubleMatrix
 *
 */ 
object DoubleMatrix extends MatrixFactory[Double]{
  
  val factory = DoubleFactory2D.dense
  import factory._
  
  def zeros(r: Int, c: Int) = new DoubleMatrix(make(r,c))
  
  def fill(r: Int, c: Int, value: => Double) = new DoubleMatrix(make(r,c,value))
  
  def eye(d: Int) = new DoubleMatrix(identity(d))
  
  def tabulate(r: Int, c: Int)(f: (Int,Int) => Double) = {
    val arr = Array.tabulate(r,c){ (x,y) => f(x,y) }
    new DoubleMatrix(make(arr))
  }

  /** creates a DoubleMatrix from any DMatrix
   *
   */ 
  def fromGeneral(that: DMatrix):DoubleMatrix = that match {
    case a:DoubleMatrix => a
    case _ => {
      tabulate(that.rows,that.cols)(that.apply)
    }
  }
}

object Linalg extends Algebra
