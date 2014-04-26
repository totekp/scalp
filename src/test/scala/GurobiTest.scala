import org.scalatest.{Matchers, GivenWhenThen, FeatureSpec}
import org.scalp.ILP

class GurobiTest extends FeatureSpec with GivenWhenThen with Matchers {

  feature("Optimize one-var LP with Gurobi") {
    scenario("minimize") {
      Given("we have an LP that minimizes one var x: 0 <= x <= 100")
      object TestILP extends ILP("Test") {
        var result = Double.PositiveInfinity

        override def run() {
          val variable = continuousVar(0, 100, "continuous var between 0 and 100")
          minimize(variable)
          result = getValue(variable)
        }
      }
      Then("we should be able to run it")
      TestILP.run()
      Then("the result should be 0")
      TestILP.result should be(0)
    }

    scenario("maximize") {
      Given("we have an LP that maximizes one var x: 0 <= x <= 100")
      object TestILP extends ILP("Test") {
        var result = Double.PositiveInfinity

        override def run() {
          val variable = continuousVar(0, 100, "continuous var between 0 and 100")
          maximize(variable)
          result = getValue(variable)
        }
      }
      Then("we should be able to run it")
      TestILP.run()
      Then("the result should be 100")
      TestILP.result should be(100)
    }
  }

}
