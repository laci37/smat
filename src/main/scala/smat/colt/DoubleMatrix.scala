package smat.colt

import smat._
import cern.colt.matrix.DoubleMatrix2D
import cern.colt.matrix.linalg.Algebra
import cern.jet.math.Functions

class DoubleMatrix(val under:DoubleMatrix2D, transpose:Boolean) extends DMatrix{

  def this(under:DoubleMatrix2D)= this(under,false)

  def rows=if(transpose) under.columns else under.rows
  def cols=if(transpose) under.rows else under.columns
  
  def apply(r:Int,c:Int)=under.get(r,c)

  def +(that:DMatrix):DMatrix={
    that match {
      case m:DoubleMatrix=>{
	new DoubleMatrix(under.copy().assign(m.under,Functions.plus))
      }
      case _ =>{
	super.+(that)
      }
    }
  }

  def -(that:DMatrix):DMatrix={
    that match {
      case m:DoubleMatrix=>{
	new DoubleMatrix(under.copy().assign(m.under,Functions.minus))
      }
      case _ =>{
	super.-(that)
      }
    }
  }

  def *(that:DMatrix):DMatrix={
    that match {
      case m:DoubleMatrix=>{
	new DoubleMatrix(Linalg.mult(this.under,m.under))
      }
      case _ =>{
	super.*(that)
      }
    }
  }

  def *(that:Double):DMatrix={
    new DoubleMatrix(under.copy().assign(Functions.mult(that)))
  }

  def t:DMatrix= new DoubleMatrix(under,!transpose)
} 

object Linalg extends Algebra
