package smat

import colt._

//trait DMatrix extends Matrix[Double]

object DMatrix extends CopyMatrixFactory[Double]{
  val parent = DoubleMatrix
}

