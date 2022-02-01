package org.marioarias



/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 1/2/22
 *         Time: 1:57 PM
 */
package object langur {
  def isType[T](value: Any, block: T => Unit): Unit = {
    value match {
      case t: T => block(t)
      case _ => fail(s"value is not ${Class[T].getClass.getSimpleName}. got=${value.getClass.getSimpleName}")
    }
  }

  def fail(message: String) = throw new RuntimeException(message)
}
