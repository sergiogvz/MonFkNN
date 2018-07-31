package skeel.classification.lazy_learning

import skeel.utils.Distance

class MonKNN(var seed:Int, k:Int, distanceType:Distance.Value=Distance.Euclidean, var neighborType:MonKNN.neighborType=MonKNN.inRange) extends skeel.classification.lazy_learning.KNN(k, distanceType) {
  protected val rand = new util.Random(seed)

  override def fit(X: Array[Array[Double]], y:Array[Int], nClassesP:Int){
    fit(X,y)
    nClasses = nClassesP
  }
  
  override def predictProba(x:Array[Double]):Array[Double] = {
      val range = yRange(x)
      val nearest = neighbors(x, range)
      val probs = new Array[Double](nClasses)
      
      if (nearest.isEmpty){
        //println("random")
        val n = range._2+1-range._1
        val sel = (rand.nextDouble*n + range._1).toInt
        for(i <- range._1 to range._2) if (i == sel) probs(i)=1.1 else probs(i)= 1d - 0.1/(n-1)
      }else for (n <- nearest) probs(Ytrain(n._1))+=1d 
      
      probs.map(_/probs.sum)
      
  }
  
  override def neighbors(x:Array[Double]):(Array[(Int,Double)]) = {
    neighbors(x, yRange(x))
  }
  
  protected def yRange(x:Array[Double]):(Int,Int) = {
    var min = 0
    var max = nClasses-1
    
    for(i <- 0 until Xtrain.length){
      if(<=(Xtrain(i),x) && Ytrain(i) > min) min = Ytrain(i)
      if(<=(x,Xtrain(i)) && Ytrain(i) < max) max = Ytrain(i)
    }
    
    if(max<min){
      min = 0 
      max = nClasses-1
    }
    
    (min,max)
  }
  
  protected def neighbors(x:Array[Double], yRange:(Int,Int)):(Array[(Int,Double)]) = {
    val nearest = Array.fill(k)(-1)
    val distA = Array.fill(k)(0.0)
    
    for (i <- 0 until Xtrain.length ){ //for instance of the training set
      if (neighborType==MonKNN.outRange || in(Ytrain(i),yRange)){
        val dist = Distance(x,Xtrain(i),distanceType)
        if (dist > 0d){
          var stop = false
          var j = 0
          while( j < k && !stop ) { //Check if it can be inserted as NN
            if ((nearest(j)==(-1) || dist < distA(j))){
              for(l <- ((j+1) until k).reverse){ //for (int l = k - 1; l >= j + 1; l--)
                nearest(l) = nearest(l-1)
                distA(l) = distA(l-1)
            }
              nearest(j) = i
              distA(j)=dist
              stop = true
            }
            j+=1
          }
        }
      }
    }
    
    (nearest zip distA) filter (i => i._1 >= 0 && in(Ytrain(i._1),yRange)) //filter (_._1>=0)
  }
  
  private def in(y:Int, yRange:(Int,Int)) = y >= yRange._1 && y<=yRange._2
  
  private def <=(a:Array[Double],b:Array[Double]) = {
    var flag = true
    var i = 0
    
    if(Distance(a,b,distanceType)==0.0) flag = false
    
    while(flag && i < a.length){
      flag = a(i) <= b(i)
      i+=1
    }
    
    
    
    flag
  }
  
}

object MonKNN extends Enumeration {
  type neighborType = Value
  val inRange, outRange = Value
}
