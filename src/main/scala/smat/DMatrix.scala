package smat

import colt._


/** default matrix factory for Matrix[Double]
 *
 *  Actually is an alias for the smat.colt.DoubleMatrix factory, which creates
 *  dense colt matrices wrapped in smat.colt.DoubleMatrix
 */
 
object DMatrix extends CopyMatrixFactory[Double]{
  val parent = DoubleMatrix
}

