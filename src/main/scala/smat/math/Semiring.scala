package smat.math

trait Semiring[A]{
  def add(a:A, b:A):A
  def mul(a:A, b:A):A
  def zero:A
  def one:A
}
