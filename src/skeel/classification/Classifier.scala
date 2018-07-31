package skeel.classification

/**
 * Baseline classifier. Every classifier must extend this class.
 * 
 * @author sergiogvz
 */
trait Classifier {
  protected var nClasses = 0
  protected var nAttributes = 0
  
  def fit(X: Array[Array[Double]], y:Array[Int]){
    nAttributes = X(0).length
    nClasses = y.max+1
    //for ()
  }

  def fit(X: Array[Array[Double]], y:Array[Int], nClassesP:Int){
    nAttributes = X(0).length
    nClasses = nClassesP
    //for ()
  }
  
  def predict(x:Array[Double]):Int = {
    var maxIndex = 0
    val probs = predictProba(x)
    
    for (i <- 1 until probs.length)
      if (probs(i) > probs(maxIndex)) maxIndex=i
      
    maxIndex
  }
  
  def predict(X:Array[Array[Double]]):Array[Int] = {
     X.map(predict(_))
  }
  
  def predictProba(x:Array[Double]):Array[Double]
  
  def predictProba(X:Array[Array[Double]]):Array[Array[Double]] = {
     X.map(predictProba(_))
  }
  
}