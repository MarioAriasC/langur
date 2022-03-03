package org.marioarias.langur.vm

import org.marioarias.langur.objects.MClosure
import org.marioarias.langur.code.Instructions

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 2/3/22
 *         Time: 9:05 AM
 */
class Frame (val cl:MClosure, val basePointer:Int){
  var ip: Int = -1

  def instructions: Instructions = cl.fn.instructions
}
