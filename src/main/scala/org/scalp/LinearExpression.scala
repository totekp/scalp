package org.scalp

import gurobi.{GRBModel, GRBLinExpr}

class LinearExpression(val terms: Set[Term], val constants: Set[Constant]=Set()) {
  def addTo(model: GRBModel) = {
    val expression = new GRBLinExpr()
    terms.foreach(term => expression addTerm (term.factor, term.variable.reference))
    constants.foreach(constant => expression addConstant constant.value)
    expression
  }
}

object LinearExpression {
  def from(terms: Set[Term]): LinearExpression = new LinearExpression(terms)
  def from(term: Term): LinearExpression = new LinearExpression(Set(term))
  def from(variable: Variable): LinearExpression = from(new Term(variable))
  def from(variables: Seq[Variable]): LinearExpression = from((variables map (v => new Term(v))).toSet)
  def from(terms: Set[Term], constants: Set[Double]): LinearExpression = new LinearExpression(terms, constants map (new Constant(_)))
  def from(terms: Set[Term], constant: Double): LinearExpression = new LinearExpression(terms, Set(new Constant(constant)))
  def from(term: Term, values: Set[Double]): LinearExpression = LinearExpression from (Set(term), values)
  def from(term: Term, constant: Double): LinearExpression = LinearExpression from (Set(term), Set(constant))
  def from(term: Term, variables: Seq[Variable]): LinearExpression = from((variables map (v => new Term(v))).toSet + term)
  def from(variable: Variable, values: Set[Double]): LinearExpression = LinearExpression from (new Term(variable), values)
  def from(variable: Variable, values: Double*): LinearExpression = LinearExpression from (new Term(variable), values.toSet)
}
