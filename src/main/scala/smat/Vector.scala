package smat

trait Vector[A] extends Matrix[A]{
  /**true if this is a row vecor*/
  def isRow:Boolean
  
  def length:Int
  
  def rows = if(isRow) 1 else length

  def cols = if(isRow) length else 1

  def apply(index:Int):A = if(isRow) apply(0,index) else apply(index,0)

  override def foreach(f: A => Unit){
    var i=0
    while(i<length){
      f(apply(i))
      i+=1
    }
  }

  def t:Vector[A]
}
