package cloud.benchflow.test.config.experiment

import cloud.benchflow.test.config._

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 11/02/16.
  */
case class VirtualUsers(virtualUsers: Int)
case class BenchFlowExperiment(name: String,
                               description: String,
                               sut: Sut,
                               virtualUsers: VirtualUsers,
                               drivers: Seq[Driver[_ <: Operation]],
                               trials: TotalTrials,
                               execution: Execution,
                               properties: Properties,
                               sutConfiguration: SutConfiguration)
{
  def getAliasForService(serviceName: String) = sutConfiguration.deploy.get(serviceName)
  def getBindingsForService(serviceName: String) = sutConfiguration.bfConfig.bindings(serviceName)
  def getBindingConfiguration(from: String, to: String): Option[Properties] =
    sutConfiguration.bfConfig.bindings(from).find(b => b.boundService == to).flatMap(_.config)
}
object BenchFlowExperiment {

  def fromYaml(yaml: String): BenchFlowExperiment = {
    import BenchFlowExperimentYamlProtocol._
    import net.jcazevedo.moultingyaml._
    yaml.stripMargin.parseYaml.convertTo[BenchFlowExperiment]
  }

}


