package skeel.utils

import scala.io.Source

class KeelDataset {
  
  var conv: Array[Map[String, Double]] = null
  var isCategorical: Array[Boolean] = null
  var numClass: Int = 0
  var numFeatures: Int = 0
  var X:Array[Array[Double]] = null
  var y:Array[Int] = null
  
  def this(filepath:String){
    this()
    val file = Source.fromFile(filepath,"UTF-8")
    val lines = file.getLines.toArray
    file.close
    val (header, body) = lines.partition(_.startsWith("@"))
    headerParser(header)
    val t = body.map(parserToFeatureClass(_)).unzip
    X=t._1.toArray; y=t._2.toArray
  }
  
  def this(hearderpath:String, bodypath:String){
    this()
    val fileHeader = Source.fromFile(hearderpath,"UTF-8")
    headerParser(fileHeader.getLines.toArray)
    fileHeader.close
    val fileBody = Source.fromFile(bodypath,"UTF-8")
    val t = fileBody.getLines.toArray.map(parserToFeatureClass(_)).unzip
    X=t._1.toArray; y=t._2.toArray
    fileBody.close()
  }
  
  
  /**
   * Parses the header and extract the main information of the features.
   *
   * @param linesHeader lines of the header
   */
  def headerParser(linesHeader:Array[String]) = {
    
    //Calculate number of features + 1 for the class
    var className = "CLASS"
    numFeatures = 0
    for (i <- 0 to (linesHeader.length - 1)) {
      if (linesHeader(i).toUpperCase().contains("@INPUTS")) { //Counting the "," + 2 (last word and class)
        numFeatures = linesHeader(i).length - linesHeader(i).replaceAllLiterally(",", "").length + 2
      } else if (linesHeader(i).toUpperCase().contains("@OUTPUT")) {
        className = linesHeader(i).split(" ")(1).toUpperCase()
      } //end if
    } //end for

    //Calculate transformation to normalize and erase categorical features
    conv = new Array[Map[String, Double]](numFeatures)
    isCategorical = new Array[Boolean](numFeatures)

    for (i <- 0 until numFeatures) {
      conv(i) = Map()
      isCategorical(i) = false
    }


    var auxParserClasses = 0.0
    var auxNumFeature = 0
    for (i <- 0 until linesHeader.length) {
      if (linesHeader(i).toUpperCase().contains("@ATTRIBUTE " + className)) {
        numClass = linesHeader(i).length - linesHeader(i).replaceAllLiterally(",", "").length + 1
        val labelsClasses = getLabels(linesHeader(i)) //Array of String with the labels of the objetive variable
        for (key <- labelsClasses) { //Calculate map for parser label classes
          conv(numFeatures - 1) += (key -> auxParserClasses)
          isCategorical(auxNumFeature) = true
          auxParserClasses = auxParserClasses + 1
        }
      } else if (linesHeader(i).toUpperCase().contains("[")) { //Real or integer feature
        val range = getRange(linesHeader(i)) //Min and max of the feature
        conv(auxNumFeature) += ("min" -> range(0), "max" -> range(1)) //Do the parser for this feature
        isCategorical(auxNumFeature) = false
        auxNumFeature = auxNumFeature + 1 //Increase for the next feature
      } else if (linesHeader(i).toUpperCase().contains("{") && !(linesHeader(i).toUpperCase().contains("@ATTRIBUTE " + className))) {
        val labelsClasses = getLabels(linesHeader(i)) //Array String with the labels of the feature
        val size = labelsClasses.length

        //Calculate the increase. If categorical variable only have a value (WTF) it must do 0 and the increase 1. Dont /0
        var inc: Double = 0.0
        if (size == 1) {
          inc = 1.0
        } else {
          inc = 1.0 / (size - 1.0)
        }

        for (i <- 0 until labelsClasses.length) { //Map to parser the label class
          conv(auxNumFeature) += (labelsClasses(i) -> i * inc)
        }
        isCategorical(auxNumFeature) = true

        auxNumFeature = auxNumFeature + 1
      } else if (linesHeader(i).toUpperCase().contains("REAL") && !(linesHeader(i).toUpperCase().contains("@ATTRIBUTE " + className))) {
        conv(auxNumFeature) += ("no-bound" -> 0, "no-bound" -> 0) //Do the parser for this feature
        isCategorical(auxNumFeature) = false
        auxNumFeature = auxNumFeature + 1 //Increase for the next feature
      }
    } //end for

  }
  
    /**
   * Get the labels of a feature or the main class as Array[String].
   *
   * @param str string to parser
   * @author Jesus Maillo
   */
  private def getLabels(str: String): Array[String] = {
    var result = str.substring(str.indexOf("{") + 1, str.indexOf("}")).replaceAll(" ", "").split(",")
    result
  }

  /**
   * Get the min and max of a feature as a Array[Double].
   *
   * @param str string to parser
   * @author Jesus Maillo
   */
  private def getRange(str: String): Array[Double] = {
    var aux = str.substring(str.indexOf("[") + 1, str.indexOf("]")).replaceAll(" ", "").split(",")
    var result = new Array[Double](2)
    result(0) = aux(0).toDouble
    result(1) = aux(1).toDouble
    result
  }
  
   /**
   * Parser a line to a Array[Double].
   *
   * @param line The string to be parsed
   * @author Jesus Maillo
   */
   private def parserToDouble(line: String): Array[Double] = {
    val size = conv.length
    val result: Array[Double] = new Array[Double](size)

    //Change the line to Array[String]
    val auxArray = line.split(",\\s*")

    //Iterate over the array parsing to double each element with the knowlegde of the header
    for (i <- 0 to size - 1) {
      if (auxArray(i) == "?") {
        result(i) = -1
      } else if (conv(i).contains("min") && conv(i).contains("max") && (conv(i).size == 2)) { //If dictionary have like key (only) min and max is real or integer, else, categorical
        result(i) = if(conv(i).get("min").get==conv(i).get("max").get) 0d
        else (auxArray(i).toDouble - conv(i).get("min").get) / (conv(i).get("max").get - conv(i).get("min").get)
      } else if (conv(i).contains("no-bound")) {
        result(i) = auxArray(i).toDouble
      } else {
        result(i) = conv(i).get(auxArray(i)).get
      }
    }

    result
  }
   
   /**
   * Parser a line to a (Array[Double], Int) (features,class).
   *
   * @param line The string to be parsed
   * @author sergiogvz
   */
   private def parserToFeatureClass(line: String): (Array[Double],Int) = {
     val result = parserToDouble(line)
   
     val d :Double = result.last
     (result.init,d.toInt)
     
  }
  
}