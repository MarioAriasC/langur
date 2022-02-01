package org.marioarias.langur.utils

import scala.language.implicitConversions

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 1/2/22
 *         Time: 10:12 AM
 */
object Utils {

  implicit class Also[T](t: T) {
    def also(body: T => Unit): T = {
      body(t)
      t
    }
  }
}
