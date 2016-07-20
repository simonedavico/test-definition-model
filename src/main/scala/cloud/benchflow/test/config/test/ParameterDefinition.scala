package cloud.benchflow.test.config.test

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  *         Created on 19/07/16.
  */
abstract class ParameterDefinition(val name: String, val scope: String, val dimensionDefinition: ValueRange[_]) {

  def completeName = s"$scope.$name"

}

//parameter definition for a service
abstract class ServiceParameterDefinition(override val name: String, serviceName: String, override val dimensionDefinition: ValueRange[_])
  extends ParameterDefinition(name, serviceName, dimensionDefinition)

//an application level parameter for a service
case class ApplicationParameterDefinition(override val name: String, serviceName: String, override val dimensionDefinition: ValueRange[_])
  extends ServiceParameterDefinition(name, serviceName, dimensionDefinition)

//memory on a service
case class ServiceMemoryDefinition(serviceName: String, override val dimensionDefinition: ValueRange[_])
  extends ServiceParameterDefinition(SystemParameterDefinition.memoryDefinitionKey, serviceName, dimensionDefinition)

//cpus on a service
case class ServiceCpusDefinition(serviceName: String, override val dimensionDefinition: ValueRange[_])
  extends ServiceParameterDefinition(SystemParameterDefinition.cpusDefinitionKey, serviceName, dimensionDefinition)


//parameter definition for the whole system (do cpus and memory make sense here?)
sealed abstract class SystemParameterDefinition(name: String, dimensionDefinition: ValueRange[_])
  extends ParameterDefinition(name, "system", dimensionDefinition)
object SystemParameterDefinition {

  val cpusDefinitionKey = "cpus"
  val memoryDefinitionKey = "memory"

}


//predefined system parameter definitions
//cpus definition
case class CpusDefinition(override val dimensionDefinition: ValueRange[_])
  extends SystemParameterDefinition(SystemParameterDefinition.cpusDefinitionKey, dimensionDefinition)

//memory definition
case class MemoryDefinition(override val dimensionDefinition: ValueRange[_])
  extends SystemParameterDefinition(SystemParameterDefinition.memoryDefinitionKey, dimensionDefinition)
