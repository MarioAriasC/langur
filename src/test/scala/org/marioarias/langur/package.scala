package org.marioarias

import scala.reflect.{ClassTag, classTag}

/** Created by IntelliJ IDEA.
 *
 * @author
 * Mario Arias Date: 1/2/22 Time: 1:57 PM
 */
package object langur {
  def checkType[T: ClassTag](value: Any)(block: T => Unit): Unit = {
    value match {
      case t: T => block(t)
      case Some(t: T) => block(t)
      case _ =>
        fail(
          s"value is not ${classTag[T].getClass.getSimpleName}. got=${value.getClass.getSimpleName}"
        )
    }
  }

  def fail(message: String) = throw new RuntimeException(message)
}
