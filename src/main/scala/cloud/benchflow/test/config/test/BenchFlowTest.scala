package cloud.benchflow.test.config.test

import cloud.benchflow.test.config._

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 18/07/16.
  */

sealed trait GoalType
case object Config extends GoalType
case object Custom extends GoalType

abstract class Goal(goalType: GoalType, params: Seq[ParameterDefinition[_]])


case class BenchFlowTest(name: String,
                         description: String,
                         sut: Sut,
                         trials: TotalTrials,
                         goal: Goal,
                         drivers: Seq[Driver[_ <: Operation]],
                         loadFunction: Execution,
                         properties: Option[Properties],
                         sutConfiguration: SutConfiguration)
