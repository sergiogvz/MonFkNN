package skeel.classification.lazy_learning

import skeel.utils.Distance

import scala.collection.mutable.WrappedArray

class MonFuzzyKNN(seed:Int, var m:Double, k:Int, distanceType:Distance.Value=Distance.Euclidean, neighborType:MonKNN.neighborType=MonKNN.inRange, penaltyOR:Double = 1d) extends skeel.classification.lazy_learning.MonKNN(seed,k,distanceType,neighborType) {
  private var Yprobs:Array[Array[Double]] = null
  private var Xset:Array[WrappedArray[Double]] = null
  private var Ymed:Array[Int] = null

  if(m==1d) m = 1.000001

  /**
    * CRIPS fit. Sets the class memberships of the
    * @param X feature vectors of training set
    * @param y class labels of the instances of training set
    */
  override def fit(X: Array[Array[Double]], y:Array[Int]) {
    super.fit(X, y)

    //Computes the CRIPS memberships
    Yprobs = Array.fill(Xtrain.length, nClasses)(0d)
    for ( i <- 0 until Ytrain.length) Yprobs(i)(Ytrain(i))=1d
  }

  /**
    * FUZZY fit. Fits the model with the class memberships given as parameter.
    * @param X feature vectors of training set
    * @param yprobs class memberships of the instances of training set
    */
  def fit(X: Array[Array[Double]], yprobs:Array[Array[Double]]) {
    //Computes the real class from probabilities and call fit of knn
    super.fit(X, Yprobs.map(i => i.zipWithIndex.maxBy(_._1)._2))
    Yprobs = yprobs
  }


  /**
    * FUZZY fit. The assignment are done related to train vs train KNN
    */
  def fitAsOSDL(X: Array[Array[Double]], y:Array[Int]) {
    super.fit(X, y)

    Xset = Xtrain.map(wrapDoubleArray(_)).toSet.toArray

    Yprobs = Xset.map(fuzzifyAsOSDL(_))
    Ymed = Yprobs.map(getMedianFromPMF(_))

  }

  /**
    * FUZZY fit. The assignment are done related to train vs train KNN
    */
  def fit(X: Array[Array[Double]], y:Array[Int], kFuzzy:Int, RCr:Double=1d) {
    fitAsOSDL(X, y)

    if(RCr!=1d) {
      val knnFuzzy = new MonKNN(seed, kFuzzy, distanceType, MonKNN.inRange)
      knnFuzzy.fit(Xset.map(_.toArray), Ymed, nClasses) //in meds sometimes classes disappear

      Yprobs = Xset.indices.toArray.map(i => if (Yprobs(i).max == 1d) fuzzifyRCr(Ymed(i),knnFuzzy.predictProba(Xset(i).toArray),RCr) else Yprobs(i))
      Ymed = Yprobs.map(getMedianFromPMF(_))
    }
  }

  /**
    * FUZZY fit. The assignment are done related to train vs train KNN
    */
  def fitasKNN(X: Array[Array[Double]], y:Array[Int], kFuzzy:Int, RCr:Double=1d) {
    fitAsOSDL(X, y)
    val knnFuzzy = new MonKNN(seed, kFuzzy, distanceType, MonKNN.inRange)
    knnFuzzy.fit(Xset.map(_.toArray), Ymed, nClasses) //in meds sometimes classes disappear

    Yprobs = Xset.indices.toArray.map(i => fuzzifyRCr(Ymed(i),knnFuzzy.predictProba(Xset(i).toArray),RCr))
    Ymed = Yprobs.map(getMedianFromPMF(_))
  }

  private def in(y:Int, yRange:(Int,Int)) = yRange._1<=y  && y<=yRange._2

  override protected def yRange(x:Array[Double]):(Int,Int) = {
    var min = 0
    var max = nClasses-1

    for(i <- Xset.indices){
      if(<=(Xset(i).toArray,x) && Ymed(i) > min) min = Ymed(i)
      if(<=(x,Xset(i).toArray) && Ymed(i) < max) max = Ymed(i)
    }

    if(max<min){
      min = 0
      max = nClasses-1
    }

    (min,max)
  }

  private def <=(a:Array[Double],b:Array[Double]) = {
    var flag = true
    var i = 0

    while(flag && i < a.length){
      flag = a(i) <= b(i)
      i+=1
    }

    flag
  }

  override protected def neighbors(x:Array[Double], yRange:(Int,Int)):(Array[(Int,Double)]) = {
    val nearest = Array.fill(k)(-1)
    val distA = Array.fill(k)(0.0)

    for (i <- Xset.indices){ //for instance of the training set
      if (neighborType==MonKNN.outRange || in(Ymed(i),yRange)){

        val dist = Distance(x,Xset(i).toArray,distanceType)
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

    (nearest zip distA) filter (i => i._1 >= 0) //filter (_._1>=0)
  }


  private def emptyOR(n:Array[(Int,Double)],range:(Int,Int)) = {
    var emp:Boolean = true

    for ((index,_) <- n){
      emp = emp && !in(Ymed(index),range)
    }

    emp
  }

  override def predictProba(x:Array[Double]):Array[Double] = {
    val range = yRange(x)
    val n = neighbors(x, range)

    if (n.isEmpty || (neighborType == MonKNN.outRange && penaltyOR == 0d && emptyOR(n, range))) {
      val tam = range._2 + 1 - range._1
      val sel = (rand.nextDouble * tam + range._1).toInt
      val probs = new Array[Double](nClasses)
      for (i <- range._1 to range._2) if (i == sel) probs(i) = 1.1 else probs(i) = 1d - 0.1 / (tam - 1)
      probs.map(_ / probs.sum)
    } else {

      val dist = n.map(i => {
        val penalty =
          if (in(Ymed(i._1),range)) 1d
          else penaltyOR

        (1.0 / math.pow(i._2, 2d / (m - 1))) * penalty
      })

      val s = dist.sum

      var ms = 0d

      val p = for (clas <- 0 until nClasses toArray) yield {
        ms = 0d
        for (i <- 0 until n.length) {
          ms += Yprobs(n(i)._1)(clas) * dist(i)
        }
        ms / s
      }

      p.map(_ / p.sum)

    }

  }


  override def predict(x: Array[Double]): Int = {
    val probs = predictProba(x)
    val med = getMedianFromPMF(probs)
    med
  }

  private def getMediansFromPMF(x: Array[Double]): Array[Int] = {
    var lowerC = -1
    var upperC = -1
    var i = 0

    var cumulative = 0d

    while( i < x.length && lowerC == -1){
      cumulative+=x(i)
      if (cumulative>=0.5) lowerC=i
      i+=1
    }

    i = x.length-1
    cumulative = 0d
    while( i >= 0 && upperC == -1){
      cumulative+=x(i)
      if (cumulative>=0.5) upperC=i
      i-=1
    }

    Array(lowerC,upperC)
  }

  private def getMedianFromPMF(x: Array[Double]): Int = {
    val meds = getMediansFromPMF(x)

    if((meds(1)-meds(0))%2==0) (meds(0)+(meds(1)-meds(0))/2d).toInt
    else{
      if(rand.nextBoolean()) meds(1)+=1
      else meds(0)-=1
      (meds(0)+(meds(1)-meds(0))/2d).toInt
    }
  }

  private def fuzzify(y:Int, probs:Array[Double]): Array[Double] = fuzzifyRCr(y,probs, 0.51)

  private def fuzzifyRCr(y:Int, probs:Array[Double], RCr:Double=0.5d): Array[Double] = {
    val probR = RCr
    val probP = 1d - probR
    for(i <- 0 until nClasses toArray) yield if (i == y) probR + probs(i)*probP else probs(i)*probP
  }

  private def fuzzifyAsOSDL(inst:WrappedArray[Double]): Array[Double] = {

    val pmf = Array.fill(nClasses)(0d)

    for (i <- Xtrain.indices)
      if (inst==wrapDoubleArray(Xtrain(i)))
        pmf(Ytrain(i))+=1

    val sum = pmf.sum
    pmf.map(_/sum)
  }

}
