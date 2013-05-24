package smat.math

trait Ring[A] extends Semiring[A]{
  def negate(a:A):A
  def sub(a:A,b:A):A={
    add(a, negate(b))
  }
}

object Ring{
  implicit val doubleRing= new Ring[Double]{
    def add(a:Double,b:Double)= a+b
    def mul(a:Double,b:Double)= a*b
    def negate(a:Double)= -a
    override def sub(a:Double, b:Double)= a-b
    val zero= 0d
    val one= 1d
  }
}
