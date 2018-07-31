package skeel.classification.lazy_learning

import skeel.utils.Distance

class KNN(var k:Int, var distanceType:Distance.Value=Distance.Euclidean) extends skeel.classification.Classifier {
  protected var Xtrain:Array[Array[Double]] = null
  protected var Ytrain:Array[Int] = null
  
  override def fit(X: Array[Array[Double]], y:Array[Int]) {
    super.fit(X, y)
    Xtrain = X
    Ytrain = y
  }
  
  def predictProba(x:Array[Double]):Array[Double] = {
      val nearest = neighbors(x)
      val probs = new Array[Double](nClasses)
      
      for (n <- nearest) probs(Ytrain(n._1))+=1d
      
      probs.map(_/k)
  }

  /**
    * Computes the Nearest Neighbours of the instance given as parameter
    * @param x feature vector of the given instance
    * @return the Nearest Neighbours as their indices in the Train set and their distances (the result is given ordered)
    */
  def neighbors(x:Array[Double]):(Array[(Int,Double)]) = {
    //val dist = Xtrain.map(Distance(x,_,distanceType))
    val nearest = Array.fill(k)(-1)
    val distA = Array.fill(k)(0.0)
    
    for (i <- 0 until Xtrain.length ){ //for instance of the training set
      var dist = Distance(x,Xtrain(i),distanceType)
      if (dist > 0d){
        var stop = false
        var j = 0
        while( j < k && !stop ) { //Check if it can be inserted as NN
          if (nearest(j)==(-1) || dist < distA(j)){
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
    
    nearest zip distA
  }
  
}