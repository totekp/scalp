package org.scalp

class LPConfiguration private[scalp]() {
  private var name = ""

  def withName(name: String) = {
    this.name = name
    this
  }

  def minimizeContinuousVariable(lowerBound: Double, upperBound: Double) = {
    var minVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        minVar = Some(continuousVar(lowerBound, upperBound, ""))
        minimize(minVar.get)
      }
    }
    lp run()
    lp getValue minVar.get
  }

  def minimizeIntegerVariable(lowerBound: Int, upperBound: Int) = {
    var minVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        minVar = Some(integerVar(lowerBound, upperBound, ""))
        minimize(minVar.get)
      }
    }
    lp run()
    (lp getValue minVar.get).toInt
  }

  def maximizeContinuousVariable(lowerBound: Double, upperBound: Double) = {
    var maxVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        maxVar = Some(continuousVar(lowerBound, upperBound, ""))
        maximize(maxVar.get)
      }
    }
    lp run()
    lp getValue maxVar.get
  }

  def maximizeIntegerVariable(lowerBound: Int, upperBound: Int) = {
    var maxVar: Option[Variable] = None
    val lp = new LP("") {
      override def run() = {
        maxVar = Some(integerVar(lowerBound, upperBound, ""))
        maximize(maxVar.get)
      }
    }
    lp run()
    (lp getValue maxVar.get).toInt
  }

}