package smat
import cern.colt.matrix.DoubleFactory2D
import colt._

//trait DMatrix extends Matrix[Double]

object DMatrix{
  val factory = DoubleFactory2D.dense
  import factory._
  def zeros(r:Int, c:Int) = new DoubleMatrix(make(r,c))
  def fill(r:Int, c:Int, value:Double) = new DoubleMatrix(make(r,c,value))
  def eye(d:Int) = new DoubleMatrix(identity(d))
}

