package cloud.benchflow.test.config.test

import net.jcazevedo.moultingyaml._

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 20/07/16.
  */
trait GoalYamlProtocol extends DefaultYamlProtocol with ParameterDefinitionYamlProtocol {

  implicit object GoalYamlFormat extends YamlFormat[Goal] {

    private def readServiceParameterDefinitions(yaml: YamlValue): Seq[ServiceParameterDefinition] = {

      val definition = yaml.asYamlObject.fields.head
      val serviceName = definition._1.convertTo[String] match {
        case onRegex(sName) => sName
      }
      val parameterDefinitions = definition._2

      parameterDefinitions match {
        case YamlArray(defs) => for {
          aDef <- defs
        }  yield ServiceParameterDefinitionYamlFormat.read(
          YamlObject(YamlString(serviceName) -> aDef)
        )
      }

    }

    val onRegex = "on\\s(.+)".r

    override def read(yaml: YamlValue): Goal = {

      val goalType = GoalType(yaml.asYamlObject.fields.get(YamlString("type")).get.convertTo[String])

      val parameterDefinitions = yaml.asYamlObject.fields.get(YamlString("parameters")).get

      val parsedDefinitions = parameterDefinitions match {
        case YamlArray(defs) => defs flatMap readServiceParameterDefinitions
      }

      Goal(
        goalType = goalType,
        params = parsedDefinitions
      )

    }

    override def write(obj: Goal): YamlValue = ???
  }

}
