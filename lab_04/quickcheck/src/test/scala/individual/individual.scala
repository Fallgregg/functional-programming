package individual

import org.junit.{Assert, Rule, Test}
import org.scalacheck.{Prop, Properties}
import org.scalacheck.Test.{check, Result, Failed, PropException}

class IndividualTaskSuite {

  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq: _*)

  @Test def `individual task works correctly`(): Unit =
    Assert.assertTrue(
      check(asProp(Individual))(identity).passed
    )

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}