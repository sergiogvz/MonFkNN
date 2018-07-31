package skeel.utils

/**
 * Factory to compute the distance between two instances.
 * @author sergiogvz
 */
object Distance extends Enumeration {
  val Euclidean, Manhattan, HVDM = Value
  
  /**
   * Computes the distance between instance x and instance y.
   * The type of the distance used is determined by the value of distanceType.
   * 
   * @param x instance x
   * @param y instance y
   * @param distanceType type of the distance used (Distance.Euclidean or Distance.Manhattan)
   */
  def apply( x : Array[Double] , y :  Array[Double], distanceType: Distance.Value=Euclidean) = {
     distanceType match {
       case Euclidean => euclidean(x,y)
       case Manhattan => manhattan(x,y)
       case _ => euclidean(x,y)
     }
  }
  
  private def euclidean( x : Array[Double] , y :  Array[Double]) = {
    var sum = 0.0
    
    //for ((i,j) <- x.zip(y) ) sum += (i-j)*(i-j)
    for (i <- 0 until x.length) sum += (x(i)-y(i))*(x(i)-y(i))
      
    Math.sqrt(sum)
    
  }
  
  private def manhattan( x : Array[Double] , y :  Array[Double]) = {
    var sum = 0.0

    //for ((i,j) <- x zip y) sum += Math.abs(i-j)
    for (i <- 0 until x.length) sum += Math.abs(x(i)-y(i))

    sum
  }

  private def hvdm( x : Array[Double] , y :  Array[Double]) = {
    var sum = 0.0

    //for ((i,j) <- x zip y) sum += Math.abs(i-j)
    for (i <- 0 until x.length) sum += Math.abs(x(i)-y(i))

    sum
  }

}