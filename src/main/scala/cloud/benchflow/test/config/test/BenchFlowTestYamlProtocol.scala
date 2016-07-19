package cloud.benchflow.test.config.test

import net.jcazevedo.moultingyaml._
import cloud.benchflow.test.config.ConfigurationYamlProtocol

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 19/07/16.
  */
object BenchFlowTestYamlProtocol extends ConfigurationYamlProtocol with ValueRangeYamlProtocol {

  implicit object GoalFormat extends YamlFormat[Goal] {

    override def read(yaml: YamlValue): Goal = {
      ???
    }

    override def write(obj: Goal): YamlValue = ???

  }



  implicit object ParameterDefinitionYamlFormat extends YamlFormat[ParameterDefinition[_]] {

    override def read(yaml: YamlValue): ParameterDefinition[_] = {
      ???
    }

    override def write(obj: ParameterDefinition[_]): YamlValue = ???

  }

}
