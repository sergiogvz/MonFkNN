package skeel.classification.lazy_learning

import skeel.utils.Distance

class FuzzyKNN(var m:Double, k:Int , distanceType:Distance.Value=Distance.Euclidean) extends skeel.classification.lazy_learning.KNN(k,distanceType) {
  private var Yprobs:Array[Array[Double]] = null
  if(m==1d) m = 1.000001
  
  /**
   * CRIPS fit
   */
  override def fit(X: Array[Array[Double]], y:Array[Int]) {
    super.fit(X, y)
    
    //Computes the CRIPS memberships
    Yprobs = Array.fill(Xtrain.length, nClasses)(0d)
    for ( i <- 0 until Ytrain.length) Yprobs(i)(Ytrain(i))=1d
  }
  
  /**
   * FUZZY fit. The memberships should be passed as parameter
   */
   def fit(X: Array[Array[Double]], yprobs:Array[Array[Double]]) {
     //Computes the real class from probabilities and call fit of knn
    super.fit(X, Yprobs.map(i => i.zipWithIndex.maxBy(_._1)._2))
    Yprobs = yprobs
  }
  
   /**
   * FUZZY fit. The memberships assignment are done related to train vs train KNN
   */
  def fitKK(X: Array[Array[Double]], y:Array[Int], kFuzzy:Int) {
    super.fit(X, y)
    
    val knnFuzzy = new KNN(kFuzzy, distanceType)
    knnFuzzy.fit(Xtrain, Ytrain)
    
    Yprobs = for (i <- 0 until Xtrain.length toArray) yield fuzzify(Ytrain(i),knnFuzzy.neighbors(Xtrain(i)).unzip._1.toArray)
    
  }
  

  
  override def predictProba(x:Array[Double]):Array[Double] = {
      val n = neighbors(x)
      
      val dist = n.map(i => 1.0/math.pow(i._2, 2d/(m-1)))
      val s = dist.sum 
      
      var ms = 0d
      for(clas <- 0 until nClasses toArray) yield {
        ms = 0d
        for(i <- 0 until k){
          ms += Yprobs(n(i)._1)(clas)*dist(i)
        }
        ms/s
      }

  }
  
  private def fuzzify(y:Int, neighbours:Array[Int]): Array[Double] = {
    val localk = neighbours.length
    
    val count = Array.fill(nClasses)(0d) //val count = new Array[Double](nClasses)
    for(i <- neighbours) count(Ytrain(i))+=1d
    
    for(i <- 0 until nClasses toArray) yield if (i == y) 0.51 + (count(i)/localk)*0.49 else (count(i)/localk)*0.49
    
  }
  
  
}