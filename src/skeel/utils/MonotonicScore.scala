package skeel.utils

   /**
   * Calculates same measurements of Score and traditional monotonic measures
   *
   * @param X Array with the data
   * @param real_pred Array of (Int,Int) with right and predicted class
   * @param nClasses Number of classes
   */
class MonotonicScore(X:Array[Array[Double]],real_pred: Array[(Int,Int)], nClasses:Int) extends Score(real_pred,nClasses) {

     /**
       * Computes the absolute number of violations in pairs.
       */
     def nViolations = {
       var nmi = 0d

       for(i <- X.indices)
         for(j <- i+1 until X.length if !isMonotonic(i,j) )
           nmi += 2.0

       nmi
     }
  
  /**
       * Computes the first version of non-monotonic index (NMI) using the data predictions.
       * Computed as p/(n*n-n) where p is the number of non-monotonic pairs and n, the total number of examples
       */
     def NMI1 = {
       var nmi = 0d

       for(i <- X.indices)
         for(j <- i+1 until X.length if !isMonotonic(i,j) )
           nmi += 2.0

       nmi/(X.length*X.length - X.length)
     }

     /**
       * Computes the second version of non-monotonic index (NMI) using the data predictions.
       * Computed as m/n where m is the number of examples with at least one monotonic violation and n, the total number of examples
       */
     def NMI2 = {
       var nmi = 0d
       val mon = Array.fill(X.length)(true)

       for(i <- 0 until X.length){ //for all which has still not found a violation
        var j = 0
         while(mon(i) && j < X.length){ //while not violated and not all checked
           if(!isMonotonic(i,j)){
             nmi+=1.0
             mon(i) = false
             if (mon(j) != false){
               nmi+=1.0
               mon(j) = false
             }
           }
           j+=1
         }
       }

       nmi/X.length
     }

     /**
       * Computes the third version of non-monotonic index (NMI) using the data predictions.
       * Computed as the number of non-monotonic pairs divided by the total comparable pairs.
       */
     def NMI3 = {
       var nmi = 0d
       var nComp = 0d

       for(i <- X.indices)
         for(j <- i+1 until X.length) {
           val state = comparableState(i, j)
           if (state != "<>") {
             nComp += 2
             if (!isMonotonic(i,j,state))
               nmi+=2
           }
         }

       if(nComp!=0) nmi/nComp
       else {
         println("NO COMPARABLE INSTANCES")
         0d
       }
     }


     private def comparableState(a:Int,b:Int) = {
       var i = 0
       // == <= >= <>
       var state = "=="

       while(i < X(a).length && state != "<>"){
         state = state match {
           case "==" => {
             if(X(a)(i) < X(b)(i)) "<="
             else if(X(a)(i) > X(b)(i)) ">="
             else "=="
           }
           case "<=" => {
             if (X(a)(i) <= X(b)(i)) "<="
             else "<>"
           }
           case ">=" => {
             if (X(a)(i) >= X(b)(i)) ">="
             else "<>"
           }
         }
         i+=1
       }

       state
     }



    private def isMonotonic(a:Int,b:Int, statep:String = "!") = {
      var i = 0
      // == <= >= <>
      val state = if (statep=="!") comparableState(a,b) else statep
      
      state match {
       case "==" => real_pred(a)._2 == real_pred(b)._2
       case "<=" => real_pred(a)._2 <= real_pred(b)._2
       case ">=" => real_pred(a)._2 >= real_pred(b)._2
       case "<>" => true
     }
  }
  
   /**
   * Computes the mean absolute error (MAE)
   */
  def MAE = {
    var err = 0.0
    for ((r,p) <- real_pred) err+=math.abs(r-p)
    err/real_pred.length
  }
  
   /**
   * Computes the mean squared error (MSE)
   */
  def MSE = {
    var err = 0.0
    for ((r,p) <- real_pred) err+=(r-p)*(r-p)
    err/real_pred.length
  }
  
   /**
    * Computes the root mean squared error (RMSE)
   */
  def RMSE = {
    var err = 0.0
    for ((r,p) <- real_pred) err+=(r-p)*(r-p)
    math.sqrt(err/real_pred.length)
  }
  
}