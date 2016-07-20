package cloud.benchflow.test.config.test

import cloud.benchflow.test.config._

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 18/07/16.
  */

sealed trait GoalType
object GoalType {

  def apply(goalType: String) = goalType match {
    case "config" => Config
    case "custom" => Custom
  }

}
case object Config extends GoalType
case object Custom extends GoalType

case class Goal(goalType: GoalType, params: Seq[ParameterDefinition])


case class BenchFlowTest(name: String,
                         description: String,
                         sut: Sut,
                         trials: TotalTrials,
                         goal: Goal,
                         drivers: Seq[Driver[_ <: Operation]],
                         loadFunction: Execution,
                         properties: Option[Properties],
                         sutConfiguration: SutConfiguration)
