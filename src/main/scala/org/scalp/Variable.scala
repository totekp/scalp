package org.scalp

import gurobi.{GRBModel, GRB}

case class Variable private (lowerBound: Double,
                             upperBound: Double,
                             varType: VARIABLE_TYPE,
                             name: String,
                             id: Int,
                             model: GRBModel
) {
  val reference = model.addVar(lowerBound, upperBound, 0, varType.value, name)

  override def toString = varType + " Variable " + id + " named " + name
}

object Variable {
  private var maxId = -1
  private def nextId() = synchronized { maxId += 1; maxId }
  private def create(lowerBound: Double, upperBound: Double, varType: VARIABLE_TYPE, name: String, model: GRBModel) = {
    synchronized {
      new Variable(lowerBound, upperBound, varType, name, nextId(), model)
    }
  }

  def count = maxId
  def binary(model: GRBModel, name: String) = create(0, 1, BINARY, name, model)
  def integer(model: GRBModel, lowerBound: Int, upperBound: Int, name: String) =
    create(lowerBound, upperBound, INTEGER, name, model)
  def continuous(model: GRBModel, lowerBound: Double, upperBound: Double, name: String) =
    create(lowerBound, upperBound, CONTINUOUS, name, model)
}

sealed trait VARIABLE_TYPE { def value:Char }
case object INTEGER extends VARIABLE_TYPE { val value=GRB.INTEGER }
case object BINARY extends VARIABLE_TYPE { val value=GRB.BINARY }
case object CONTINUOUS extends VARIABLE_TYPE { val value=GRB.CONTINUOUS }
