package cloud.benchflow.test.deployment.docker.service

import org.scalatest.{FlatSpec, Matchers}

/**
  * @author Simone D'Avico (simonedavico@gmail.com)
  *
  * Created on 18/07/16.
  */
class ServiceSpec extends FlatSpec with Matchers {

  import cloud.benchflow.test.deployment.docker.service.ServiceYamlProtocol._
  import net.jcazevedo.moultingyaml._

  import scala.collection.mutable.{Map => MutableMap}

  "Docker Service" should "parse correctly" in {

    val service =
      """
        |camunda:
        |  image: camunda_image
        |  cpuset: 0,1,2,3
        |  mem_limit: 5g
        |  environment:
        |    - constraint:node==bull
        |    - VAR=5
        |    - OTHER_VAR=http://google.com
      """.stripMargin.parseYaml.convertTo[Service]

    val parsedService = Service(
      name = "camunda",
      image = Some(Image("camunda_image")),
      environment = Environment(
        MutableMap(
          "constraint" -> "bull",
          "VAR" -> "5",
          "OTHER_VAR" -> "http://google.com"
        )
      ),
      memLimit = Some(MemLimit(limit = 5, unit = GigaByte)),
      cpuSet = Some(CpuSet(4))
    )

    service should be (parsedService)

  }



}
