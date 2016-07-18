package cloud.benchflow.test.config.test

import cloud.benchflow.test.config._

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 18/07/16.
  */
abstract class Goal
case class BenchFlowTest(name: String,
                         description: String,
                         sut: Sut,
                         trials: TotalTrials,
                         goal: Goal,
                         drivers: Seq[Driver[_ <: Operation]],
                         loadFunction: Execution,
                         properties: Option[Properties],
                         sutConfiguration: SutConfiguration)
