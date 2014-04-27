import org.scalatest.{Matchers, GivenWhenThen, FeatureSpec}
import org.scalp.ILP

class OneVarTest extends FeatureSpec with GivenWhenThen with Matchers {
  val lowerBound = -0.4
  val upperBound = 100.4

  feature("Optimize one-continuous-var LP with Gurobi") {
    scenario("minimize") {
      Given("we have an LP that minimizes one continuous " + varDesc)
      object TestILP extends ILP("Test") {
        var result = Double.PositiveInfinity

        override def run() {
          val variable = continuousVar(lowerBound, upperBound, varDesc)
          minimize(variable)
          result = getValue(variable)
        }
      }
      Then("we should be able to run it")
      TestILP.run()
      Then("the result should be " + lowerBound)
      TestILP.result should be (lowerBound)
    }

    scenario("maximize") {
      Given("we have an LP that maximizes one continuous" + varDesc)
      object TestILP extends ILP("Test") {
        var result = Double.PositiveInfinity

        override def run() {
          val variable = continuousVar(lowerBound, upperBound, varDesc)
          maximize(variable)
          result = getValue(variable)
        }
      }
      Then("we should be able to run it")
      TestILP.run()
      Then("the result should be " + upperBound)
      TestILP.result should be (upperBound)
    }
  }

  feature("Optimize one-integer-var LP with Gurobi") {
    scenario("minimize") {
      Given("we have an LP that minimizes one integer " + varDesc)
      object TestILP extends ILP("Test") {
        var result = Double.PositiveInfinity

        override def run() {
          val variable = integerVar(lowerBound.toInt, upperBound.toInt, varDesc)
          minimize(variable)
          result = getValue(variable)
        }
      }
      Then("we should be able to run it")
      TestILP.run()
      Then("the result should be 0")
      TestILP.result should be (lowerBound.toInt)
    }

    scenario("maximize") {
      Given("we have an LP that maximizes one integer " + varDesc)
      object TestILP extends ILP("Test") {
        var result = Double.PositiveInfinity

        override def run() {
          val variable = integerVar(lowerBound.toInt, upperBound.toInt, varDesc)
          maximize(variable)
          result = getValue(variable)
        }
      }
      Then("we should be able to run it")
      TestILP.run()
      Then("the result should be 100")
      TestILP.result should be (upperBound.toInt)
    }
  }

  def varDesc = "var x: " + lowerBound + " <= x <= " + upperBound
}
