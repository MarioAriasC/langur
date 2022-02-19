package org.marioarias.langur.evaluator

import org.marioarias.langur.objects.MObject
import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 19/2/22
 *         Time: 5:22 PM
 */
class Environment private(val store: mutable.HashMap[String, MObject], val outer: Option[Environment]) {

}

object Environment {
  def newEnvironment(): Environment = Environment(mutable.HashMap.empty, None)

  def newEnclosedEnvironment(outer: Environment): Environment = Environment(mutable.HashMap.empty, Some(outer))
}
