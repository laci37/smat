package smat

trait MatrixFactory[A]{
  
  def zeros(r: Int, c: Int):Matrix[A]
  
  def fill(r: Int, c: Int, value: => A):Matrix[A]
  
  def eye(d: Int):Matrix[A]
  
  def tabulate(r: Int, c: Int)(f: (Int,Int) => A):Matrix[A]
}

trait CopyMatrixFactory[A] extends MatrixFactory[A]{
  
  def parent:MatrixFactory[A]

  def zeros(r: Int, c: Int) = parent.zeros(r,c)
  
  def fill(r: Int, c: Int, value: => A) = parent.fill(r,c,value)
  
  def eye(d: Int) = parent.eye(d)
  
  def tabulate(r: Int, c: Int)(f: (Int,Int) => A) = parent.tabulate(r,c)(f)

}
