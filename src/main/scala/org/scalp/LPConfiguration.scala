package org.scalp

private[scalp] class LPConfiguration(private var name: String = "") {

  def withName(name: String) = {
    this.name = name
    this
  }

  def minimizeContinuousVariable(lowerBound: Double, upperBound: Double): Double = {
    var optMinVar: Option[Variable] = None

    ScalpUtil.withClose{
      new LP("") {
        override def run() = {
          optMinVar = Some(continuousVar(lowerBound, upperBound, ""))
          minimize(optMinVar.getOrElse(throw new Exception("No minVar")))
        }
      }
    }({ case lp =>
        lp.run()
        lp.getValue(optMinVar.getOrElse(throw new Exception("No minVar")))
    }, {
      _.dispose()
    })
  }

  def minimizeIntegerVariable(lowerBound: Int, upperBound: Int): Int = {
    var optMinVar: Option[Variable] = None

    ScalpUtil.withClose {
      new LP("") {
        override def run() = {
          optMinVar = Some(integerVar(lowerBound, upperBound, ""))
          minimize(optMinVar.getOrElse(throw new Exception("No minVar")))
        }
      }
    }({ case lp =>
      lp.run()
      (lp getValue optMinVar.getOrElse(throw new Exception("No minVar"))).toInt
    }, {
        _.dispose()
      }
    )
  }

  def maximizeContinuousVariable(lowerBound: Double, upperBound: Double): Double = {
    var optMaxVar: Option[Variable] = None
    ScalpUtil.withClose{
      new LP("") {
        override def run() = {
          optMaxVar = Some(continuousVar(lowerBound, upperBound, ""))
          maximize(optMaxVar.getOrElse(throw new Exception("No maxVar")))
        }
      }
    }({ case lp =>
        lp run()
        lp getValue optMaxVar.getOrElse(throw new Exception("No maxVar"))
    }, {
      _.dispose()
    })
  }

  def maximizeIntegerVariable(lowerBound: Int, upperBound: Int): Int = {
    var optMaxVar: Option[Variable] = None
    ScalpUtil.withClose {
      new LP("") {
        override def run() = {
          optMaxVar = Some(integerVar(lowerBound, upperBound, ""))
          maximize(optMaxVar.getOrElse(throw new Exception("No maxVar")))
        }
      }
    }({ case lp =>
      lp run()
      (lp getValue optMaxVar.getOrElse(throw new Exception("No maxVar"))).toInt
    }, {
      _.dispose()
    })
  }

}