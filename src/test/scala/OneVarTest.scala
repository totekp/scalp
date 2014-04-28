import org.scalatest.{Matchers, GivenWhenThen, FeatureSpec}
import org.scalp.{Constraint, LP}

class OneVarTest extends FeatureSpec with GivenWhenThen with Matchers {
  val lowerBound = -0.4
  val upperBound = 100.4

  val varDesc = "var x: " + lowerBound + " <= x <= " + upperBound

  feature("Optimize one-continuous-var LP with Gurobi") {
    scenario("minimize") {
      Given("we run an LP that minimizes one continuous " + varDesc)
      val result = LP minimizeContinuousVariable (lowerBound, upperBound)
      Then("the result should be " + lowerBound)
      result should be (lowerBound)
    }

    scenario("maximize") {
      Given("we run an LP that maximizes one continuous " + varDesc)
      val result = LP maximizeContinuousVariable (lowerBound, upperBound)
      Then("the result should be " + upperBound)
      result should be (upperBound)
    }
  }

  feature("Optimize one-integer-var LP with Gurobi") {
    scenario("minimize") {
      Given("we run an LP that minimizes one integer " + varDesc)
      val result = LP minimizeIntegerVariable(lowerBound.toInt, upperBound.toInt)
      Then("the result should be " + lowerBound.toInt)
      result should be(lowerBound.toInt)
    }

    scenario("maximize") {
      Given("we run an LP that maximizes one integer " + varDesc)
      val result = LP maximizeIntegerVariable(lowerBound.toInt, upperBound.toInt)
      Then("the result should be " + upperBound.toInt)
      result should be(upperBound.toInt)
    }

    scenario("minimize with one limiting constraint") {
      Given("We run an LP that minimizes one integer " + varDesc + " larger than 5.6")
      var result = 0
      new LP("Test") {
        override def run() {
          val intVar = integerVar(lowerBound.toInt, upperBound.toInt, "test variable");
          add((Constraint limiting intVar) >= 5.6 named "test constraint")
          minimize(intVar)
          result = getValue(intVar).toInt
        }
      }.run()
      Then("the result should be 6")
      result should be(6)
    }
  }
}
