package org.marioarias

import org.marioarias.langur.ast.Program
import org.marioarias.langur.code.*
import org.marioarias.langur.lexer.Lexer
import org.marioarias.langur.objects.{MInteger, MObject, MString, typeDesc}
import org.marioarias.langur.parser.Parser
import utest.ArrowAssert

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

  extension (l: List[Instructions]) {
    def concatInstructions: Instructions = l.fold(Array[UB]()) { (acc, bytes) => acc ++ bytes }
  }

  def parse(input: String): Program = {
    Parser(Lexer(input)).parseProgram()
  }

  def assertInstructions(expected: Instructions, actual: Instructions): Unit = {
//    println("expected" + expected.mkString("Array(", ", ", ")"))
//    println("actual" + actual.mkString("Array(", ", ", ")"))
    expected.zipWithIndex.foreach { (byte, i) =>
      byte ==> actual(i)
    }
  }

  def testIntegerObject(expected: Long, actual: MObject): Unit = {
    actual match {
      case i: MInteger => expected ==> i.value
      case _ => fail(s"object is not Integer. got=${actual.typeDesc()}")
    }
  }

  def testStringObject(expected: String, actual: MObject): Unit = {
    actual match {
      case s: MString => expected ==> s.value
      case _ => fail(s"object is not String. got=${actual.typeDesc()}")
    }
  }
}
