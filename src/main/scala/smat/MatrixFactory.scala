package smat

/** Base trait for all objects creating matrices
 *
 */ 
trait MatrixFactory[A]{
  
  /** creates new r*c matrix filled with zeros 
   *
   */ 
  def zeros(r: Int, c: Int):Matrix[A]
  
  /** creates new r*c matrix filled with value 
   *
   */
  def fill(r: Int, c: Int, value: => A):Matrix[A]
  
  /** creates new d*d identity matrix 
   *
   */
  def eye(d: Int):Matrix[A]
  
  /** fills an r*c matrix with values provided by f
   *
   *  $f(i,j)$ should equal $M_{ij}$
   */ 
  def tabulate(r: Int, c: Int)(f: (Int,Int) => A):Matrix[A]
}

/** implements a matrix factory which is just an alias for parent
 *
 */ 
trait CopyMatrixFactory[A] extends MatrixFactory[A]{
  
  def parent:MatrixFactory[A]

  def zeros(r: Int, c: Int) = parent.zeros(r,c)
  
  def fill(r: Int, c: Int, value: => A) = parent.fill(r,c,value)
  
  def eye(d: Int) = parent.eye(d)
  
  def tabulate(r: Int, c: Int)(f: (Int,Int) => A) = parent.tabulate(r,c)(f)

}
