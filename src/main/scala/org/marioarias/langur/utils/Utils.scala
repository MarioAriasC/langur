package org.marioarias.langur.utils

/** Created by IntelliJ IDEA.
  *
  * @author
  *   Mario Arias Date: 1/2/22 Time: 10:12 AM
  */
object Utils {

  extension[T](t: T) {
    def also(body: T => Unit): T = {
      body(t)
      t
    }
  }
}
