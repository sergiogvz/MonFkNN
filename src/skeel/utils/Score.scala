package skeel.utils

   /**
   * Calculate the confusion matrix and different measurements 
   *
   * @param real_pred Array of (Int,Int) with right and predicted class
   * @param nClasses Number of classes
   */
class Score(real_pred: Array[(Int,Int)], nClasses:Int) {
  val confusionMatrix = Array.fill(nClasses,nClasses)(0)
  for ((r,p) <- real_pred)
    confusionMatrix(p)(r) += 1

  
    
  /**
   * Calculate the confusion matrix and different measurements 
   *
   * @param real Array with real classes
   * @param pred Array with predicted classes
   * @param nClasses Number of classes
   */  
  def this(real:Array[Int], pred:Array[Int], nClasses:Int){
    this(real zip pred, nClasses)
  }
  
  /**
   * Computes the accuracy
   */
  def accuracy = {
    var acc = 0.0
    for ((r,p) <- real_pred) if(r==p) acc+=1.0
    acc/real_pred.length
  }
  
  /**
   * Computes the error (1.0 - accuracy)
   */
  def error = 1.0 - accuracy

   /**
     * Computes the Accuracy per Class
     */
   def accPerClass = {

     val accs = Array.fill(nClasses)(0d)
     val insts = Array.fill(nClasses)(0)

     for ((r,p) <- real_pred){
       if(r==p)
         accs(r)+=1.0

       insts(r)+=1
     }

     accs.indices.map(i => accs(i)/insts(i)).toArray

   }

   /**
     * Computes the Macro Average Arithmetic
     */
   def MAvA = accPerClass.sum / nClasses



  
}