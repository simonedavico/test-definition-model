package cloud.benchflow.test.config.test

import net.jcazevedo.moultingyaml._

import scala.util.Try


/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 20/07/16.
  */
trait ParameterDefinitionYamlProtocol extends DefaultYamlProtocol with ValueRangeYamlProtocol {

  implicit object MemoryDefinitionYamlFormat extends YamlFormat[MemoryDefinition] {

    override def read(yaml: YamlValue): MemoryDefinition = {

      val valueRange = yaml.asYamlObject.fields.get(YamlString(SystemParameterDefinition.memoryDefinitionKey))
                           .get.convertTo[ValueRange[_]]

      MemoryDefinition(valueRange)

    }

    override def write(obj: MemoryDefinition): YamlValue = ???
  }


  implicit object CpusDefinitionYamlFormat extends YamlFormat[CpusDefinition] {

    override def read(yaml: YamlValue): CpusDefinition = {

      val valueRange = yaml.asYamlObject.fields.get(YamlString(SystemParameterDefinition.cpusDefinitionKey))
                           .get.convertTo[ValueRange[_]]

      CpusDefinition(valueRange)

    }

    override def write(obj: CpusDefinition): YamlValue = ???
  }

  implicit object SystemParameterDefinitionYamlFormat extends YamlFormat[SystemParameterDefinition] {

    override def read(yaml: YamlValue): SystemParameterDefinition = {

      Try(MemoryDefinitionYamlFormat.read(yaml))
        .recover[SystemParameterDefinition] {

          case _ => CpusDefinitionYamlFormat.read(yaml)

        }.get

    }

    override def write(obj: SystemParameterDefinition): YamlValue = ???
  }


  implicit object ServiceParameterDefinitionYamlFormat extends YamlFormat[ServiceParameterDefinition] {

    override def read(yaml: YamlValue): ServiceParameterDefinition = {

      val serviceName = yaml.asYamlObject.fields.head._1.convertTo[String]
      val paramDefinition = yaml.asYamlObject.fields.head._2.asYamlObject
      val paramName = paramDefinition.fields.head._1.convertTo[String]
      val valueRange = paramDefinition.fields.head._2.convertTo[ValueRange[_]]

      paramName match {

        case SystemParameterDefinition.memoryDefinitionKey => ServiceMemoryDefinition(
          serviceName = serviceName,
          dimensionDefinition = valueRange
        )

        case SystemParameterDefinition.cpusDefinitionKey => ServiceCpusDefinition(
          serviceName = serviceName,
          dimensionDefinition = valueRange
        )

        case _ =>

          ApplicationParameterDefinition(
            name = paramName,
            serviceName = serviceName,
            dimensionDefinition = valueRange
          )
      }

    }

    override def write(obj: ServiceParameterDefinition): YamlValue = ???

  }

}
