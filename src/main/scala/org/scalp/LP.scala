package org.scalp

import gurobi.GRB.{DoubleAttr, StringAttr}
import gurobi._

abstract class LP(val name: String, val environment: GRBEnv = new GRBEnv()) extends ModelPart(new GRBModel(environment)) {
  model.set(StringAttr.ModelName, name)
  private var parts = Set[ModelPart](this)

  private var objectiveSet = false
  private var optimized = false

  model.getEnv.set(GRB.DoubleParam.IntFeasTol, 0.01)

  def run()

  def add(part: ModelPart) {
    synchronized(parts += part)
  }

  override def dispose()  {
    environment.dispose()
    super.dispose()
  }

  private def minimize(objective: GRBLinExpr) {
    model.setObjective(objective, GRB.MINIMIZE)
    objectiveSet = true
  }

  private def maximize(objective: GRBLinExpr) {
    model.setObjective(objective, GRB.MAXIMIZE)
    objectiveSet = true
  }

  def minimize(objective: LinearExpression) {
    if(objectiveSet) throw new IllegalStateException("objective already set")
    minimize(objective.addTo(model))
  }
  def maximize(objective: LinearExpression) {
    if(objectiveSet) throw new IllegalStateException("objective already set")
    maximize(objective.addTo(model))
  }

  def minimize(variable: Variable) { minimize(LinearExpression from variable) }
  def maximize(variable: Variable) { maximize(LinearExpression from variable) }
  def maximize(variables: Seq[Variable]) { maximize(LinearExpression from variables)  }
  def minimize(variables: Seq[Variable]) { minimize(LinearExpression from variables)  }

  def optimize() {
    if(!objectiveSet) throw new IllegalStateException("objective not set")
    if(!optimized) {
      model.optimize()
      optimized = true
    }
  }

  def timeLimit(seconds: Long) {
    println("setting time limit to " + seconds + " s")
    model.getEnv.set(GRB.DoubleParam.TimeLimit, seconds)
  }

  def getPositiveBinaryVars(variables: Variable*) = {
    optimize()
    getVarsWithValues(variables).filter(v => math.round(v._2) == 1.0).unzip._1
  }

  def getVarsWithValues(variables: Seq[Variable]): Seq[(Variable, Double)] = {
    variables zip (variables map (variable => getValue(variable.reference)))
  }

  def printVars(variable: Variable*) {
    optimize()
    variable foreach printVar
  }

  def printVar(id: Int) {
    optimize()
    printVar(model.getVar(id))
  }

  private def printVar(variable: Variable) {
    printVar(variable.reference)
  }

  private def printVar(grbVar: GRBVar) { println(grbVar.get(StringAttr.VarName) + " " + getValue(grbVar)) }

  private def getValue(grbVar: GRBVar) = { optimize(); grbVar.get(DoubleAttr.X) }

  def getValue(variable: Variable): Double = getValue(variable.reference)

  def getValue(id: Int): Double = getValue(model.getVar(id))
}

object LP {
  def minimizeContinuousVariable(lowerBound: Double, upperBound: Double) =
    new LPConfiguration minimizeContinuousVariable(lowerBound, upperBound)
  def minimizeIntegerVariable(lowerBound: Int, upperBound: Int) =
    new LPConfiguration minimizeIntegerVariable(lowerBound, upperBound)
  def maximizeContinuousVariable(lowerBound: Double, upperBound: Double) =
    new LPConfiguration maximizeContinuousVariable(lowerBound, upperBound)
  def maximizeIntegerVariable(lowerBound: Int, upperBound: Int) =
    new LPConfiguration maximizeIntegerVariable(lowerBound, upperBound)
}

