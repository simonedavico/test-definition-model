package cloud.benchflow.test.config.test

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  *         Created on 19/07/16.
  */
abstract class ParameterDefinition[T](name: String, dimensionDefinition: ValueRange[T])

//parameter definition for a service
class ServiceParameterDefinition[T](name: String, serviceName: String, dimensionDefinition: ValueRange[T])
  extends ParameterDefinition[T](name, dimensionDefinition)

//parameter definition for the whole system
sealed abstract class SystemParameterDefinition[T](name: String, dimensionDefinition: ValueRange[T])
  extends ParameterDefinition[T](name, dimensionDefinition)
object SystemParameterDefinition {

  val cpusDefinitionKey = "cpus"
  val memoryDefinitionKey = "memory"

}


//predefined system parameter definitions
//cpus definition
class CpusDefinition(dimensionDefinition: ValueRange[Int])
  extends SystemParameterDefinition[Int](SystemParameterDefinition.cpusDefinitionKey, dimensionDefinition)

//memory definition
class MemoryDefinition(dimensionDefinition: ValueRange[Int])
  extends SystemParameterDefinition[Int](SystemParameterDefinition.memoryDefinitionKey, dimensionDefinition)
