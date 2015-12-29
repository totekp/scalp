package org.scalp

private[scalp] class LPConfiguration(private var name: String = "") {

  def withName(name: String) = {
    this.name = name
    this
  }

  def minimizeContinuousVariable(lowerBound: Double, upperBound: Double): Double = {
    var optMinVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        optMinVar = Some(continuousVar(lowerBound, upperBound, ""))
        minimize(optMinVar.getOrElse(throw new Exception("No minVar")))
      }
    }
    lp run()
    lp getValue optMinVar.getOrElse(throw new Exception("No minVar"))
  }

  def minimizeIntegerVariable(lowerBound: Int, upperBound: Int): Int = {
    var optMinVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        optMinVar = Some(integerVar(lowerBound, upperBound, ""))
        minimize(optMinVar.getOrElse(throw new Exception("No minVar")))
      }
    }
    lp run()
    (lp getValue optMinVar.getOrElse(throw new Exception("No minVar"))).toInt
  }

  def maximizeContinuousVariable(lowerBound: Double, upperBound: Double): Double = {
    var optMaxVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        optMaxVar = Some(continuousVar(lowerBound, upperBound, ""))
        maximize(optMaxVar.getOrElse(throw new Exception("No maxVar")))
      }
    }
    lp run()
    lp getValue optMaxVar.getOrElse(throw new Exception("No maxVar"))
  }

  def maximizeIntegerVariable(lowerBound: Int, upperBound: Int): Int = {
    var optMaxVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        optMaxVar = Some(integerVar(lowerBound, upperBound, ""))
        maximize(optMaxVar.getOrElse(throw new Exception("No maxVar")))
      }
    }
    lp run()
    (lp getValue optMaxVar.getOrElse(throw new Exception("No maxVar"))).toInt
  }

}