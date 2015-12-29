package org.scalp

import gurobi.{GRB, GRBModel}

abstract class ModelPart(protected val model: GRBModel) {
  private var constraints: Int = 0

  def variableCount: Int = Variable.count
  def constraintCount: Int = constraints

  def add(constraint: Constraint) {
    constraint addTo model
    synchronized(constraints += 1)
  }

  def addMultiple(constraints: Set[Constraint]) {
    constraints foreach add
  }

  def synchronizeVars() { model.update() }

  def const(constant: Double): Constant = new Constant(constant)

  private def synchronize(variable: Variable): Variable = {
    model.update()
    variable
  }

  def integerVar(lowerBound: Int, upperBound: Int, name:  String) =
    synchronize (Variable integer (model, lowerBound, upperBound, name))

  def binaryVar(name: String) = synchronize(Variable binary (model, name))

  def sos1LinearIncreasing(vars: Array[Variable]) = sos1(vars, linearIncreasingWeightsFor(vars))

  def sos1LinearDecreasing(vars: Array[Variable]) = sos1(vars, linearDecreasingWeightsFor(vars))

  def sos1(vars: Array[Variable], weights: Array[Double]) {
    require(vars.length == weights.length)
    model.addSOS(vars map (_.reference), weights, GRB.SOS_TYPE1)
  }

  private def linearIncreasingWeightsFor(vars: Array[Variable]) = ((0 until vars.length) map (_.toDouble)).toArray

  private def linearDecreasingWeightsFor(vars: Array[Variable]) = linearIncreasingWeightsFor(vars).reverse

  def continuousVar(lowerBound: Double, upperBound: Double, name: String) =
    synchronize(asynchronousContinuousVar(lowerBound, upperBound, name))

  def asynchronousBinaryVar(name: String) = Variable binary (model, name)

  def asynchronousIntegerVar(lowerBound: Int, upperBound: Int, name: String) =
    Variable integer (model, lowerBound, upperBound, name)

  def asynchronousContinuousVar(lowerBound: Double, upperBound: Double, name: String) =
    Variable continuous (model, lowerBound, upperBound, name)
}
