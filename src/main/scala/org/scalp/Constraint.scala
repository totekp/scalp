package org.scalp

import gurobi.{GRB, GRBModel}

class Constraint private (val expression: LinearExpression, val operator: CONSTRAINT_OPERATOR, val rhs: Double,
                 val name: String) {
  var added = false

  def addTo(model: GRBModel) {
    if(!added) {
      model.addConstr(expression.addTo(model), operator.value, rhs, name)
      added = true
    }
  }
}

object Constraint {
  private var expr: Option[LinearExpression] = None
  private var operator: Option[CONSTRAINT_OPERATOR] = None
  private var rhs: Option[Double] = None

  def limiting(expression: LinearExpression): this.type = { expr=Some(expression); this }
  def limiting(const: Double, terms: Set[Term]) = { expr=Some(LinearExpression from (terms, const)); this}
  def limiting(const: Double, vars: (Variable, Double)*) = {
    val terms = vars.map(t => new Term(t._1, t._2)).toSet
    expr=Some(LinearExpression from (terms, const))
    this
  }
  def limiting(v: Variable, factor: Double) : this.type = limiting(0d, (v, factor))
  def limiting(vars: Variable*) : this.type = limiting(0d, vars.map(v => (v, 1d)):_*)

  def <=(d: Double) = { operator=Some(LESS_EQUAL); rhs=Some(d); this }
  def >=(d: Double) = { operator=Some(GREATER_EQUAL); rhs=Some(d); this}
  def >(i: Integer) = { operator=Some(GREATER_EQUAL); rhs=Some(i+1); this}
  def <(i: Integer) = { operator=Some(LESS_EQUAL); rhs=Some(i-1); this}
  def ==(d: Double) = { operator=Some(EQUAL); rhs=Some(d); this}
  def ==(i: Int) = { operator=Some(EQUAL); rhs=Some(i); this}
  def named(name: String): Constraint = new Constraint(expr.get, operator.get, rhs.get, name)
}

sealed trait CONSTRAINT_OPERATOR { def value:Char }
case object LESS_EQUAL extends CONSTRAINT_OPERATOR { override val value=GRB.LESS_EQUAL }
case object GREATER_EQUAL extends CONSTRAINT_OPERATOR { override val value=GRB.GREATER_EQUAL }
case object EQUAL extends CONSTRAINT_OPERATOR { override val value=GRB.EQUAL }
