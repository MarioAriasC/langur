package org.marioarias.langur.objects

import org.marioarias.langur.ast.*
import org.marioarias.langur.evaluator.Environment
import org.marioarias.langur.code.Instructions

type BuiltinFunction = List[Option[MObject]] => Option[MObject]

trait MObject {
  def inspect(): String
}

trait MValue[T](val value: T) extends MObject {
  override def inspect(): String = value.toString
}

enum HashType {
  case INTEGER, BOOLEAN, STRING
}

case class HashKey(hashType: HashType, value: Int)

trait MHashable[T] extends MValue[T] {
  def hashType: HashType

  def hashKey(): HashKey = HashKey(hashType, value.hashCode())
}

class MInteger(value: Long) extends MValue[Long](value) with MHashable[Long] with Ordered[MInteger] {
  override def compare(that: MInteger): Int = value.compareTo(that.value)

  def +(other: MInteger): MInteger = MInteger(value + other.value)

  def /(other: MInteger): MInteger = MInteger(value / other.value)

  def -(other: MInteger): MInteger = MInteger(value - other.value)

  def *(other: MInteger): MInteger = MInteger(value * other.value)

  def unary_- : MInteger = MInteger(-value)

  override def equals(obj: Any): Boolean = {
    obj match {
      case other: MInteger => value == other.value
      case _ => false
    }
  }

  override def hashCode(): Int = value.hashCode()

  override def toString: String = s"MInteger(value=$value)"

  override def hashType: HashType = HashType.INTEGER
}

class MError(val message: String) extends MObject {
  override def inspect(): String = s"ERROR: $message"

  override def toString: String = s"MError(message='$message')"
}

class MReturnValue(val value: MObject) extends MObject {
  override def inspect(): String = value.inspect()
}

class MBoolean(value: Boolean) extends MValue[Boolean](value) with MHashable[Boolean] {
  override def hashType: HashType = HashType.BOOLEAN

  override def hashCode(): Int = value.hashCode()

  override def equals(obj: Any): Boolean = {
    if (obj.hashCode() == this.hashCode()) return true
    if (!obj.isInstanceOf[MBoolean]) return false
    obj.asInstanceOf[MBoolean].value == value
  }

  override def toString: String = s"MBoolean(value=$value)"
}

object MNull extends MObject {
  override def inspect(): String = "null"
}

class MFunction(val parameters: Option[List[Identifier]], val body: Option[BlockStatement], val env: Environment) extends MObject {
  override def inspect(): String = s"fn(${parameters.map(_.map(_.toString)).getOrElse(List.empty).mkString(",")}) {\n\t${body.debug()}\n}"
}

class MString(value: String) extends MValue[String](value) with MHashable[String] {
  override def hashType: HashType = HashType.STRING

  def +(that: MString): MString = MString(value + that.value)

  override def toString: String = s"MString(value=\"$value\")"
}

class MArray(val elements: List[Option[MObject]]) extends MObject {
  override def inspect(): String = s"[${elements.map(_.debug()).mkString(", ")}]"
}

case class HashPair(key: MObject, value: MObject)

class MHash(val pairs: Map[HashKey, HashPair]) extends MObject {
  override def inspect(): String = s"{${pairs.values.map(pair => s"${pair.key.inspect()}: ${pair.value.inspect()}").mkString(", ")}}"
}

class MBuiltinFunction(val fn: BuiltinFunction) extends MObject {
  override def inspect(): String = "builtin function"
}

class MCompiledFunction(val instructions: Instructions, val numLocals: Int = 0, val numParameters: Int = 0) extends MObject {
  override def inspect(): String = s"CompiledFunction[$this]"
}

class MClosure(val fn:MCompiledFunction, val free:List[MObject] = List.empty) extends MObject{
  override def inspect(): String = s"Closure[$this]"
}

extension (m: Option[MObject]) {
  def typeDesc(): String = m match {
    case Some(v) => v.typeDesc()
    case None => "null"
  }
}

extension (m: MObject) {
  def typeDesc(): String = m.getClass.getSimpleName
}

private def argSizeCheck(expectedSize: Int, args: List[Option[MObject]])(body: BuiltinFunction): Option[MObject] = {
  val length = args.size
  if (length != expectedSize) {
    Some(MError(s"wrong number of arguments. got=$length, want=$expectedSize"))
  } else {
    body(args)
  }
}

private def arrayCheck(name: String, args: List[Option[MObject]])(body: (MArray, Int) => Option[MObject]): Option[MObject] = {
  args.head match {
    case Some(array: MArray) => body(array, array.elements.size)
    case nonArray: _ => Some(MError(s"argument to `$name` must be ARRAY, got ${nonArray.typeDesc()}"))
  }
}

val builtins = List(
  "len" -> MBuiltinFunction {
    argSizeCheck(1, _) { args =>
      args.head match {
        case Some(s: MString) => Some(MInteger(s.value.length.toLong))
        case Some(a: MArray) => Some(MInteger(a.elements.size.toLong))
        case e: _ => Some(MError(s"argument to `len` not supported, got ${e.typeDesc()}"))
      }
    }
  },
  "puts" -> MBuiltinFunction{ args =>
    args.foreach(arg => println(arg.map(_.inspect()).getOrElse("null")))
    None
  },
  "first" -> MBuiltinFunction { args =>
    argSizeCheck(1, args) {
      arrayCheck("first", _) { (array, length) =>
        if (length > 0) {
          array.elements.head
        } else {
          None
        }
      }
    }
  },
  "last" -> MBuiltinFunction { args =>
    argSizeCheck(1, args) {
      arrayCheck("last", _) { (array, length) =>
        if (length > 0) {
          array.elements.last
        } else {
          None
        }
      }
    }
  },
  "rest" -> MBuiltinFunction { args =>
    argSizeCheck(1, args) {
      arrayCheck("rest", _) { (array, length) =>
        if (length > 0) {
          Some(MArray(array.elements.drop(1)))
        } else {
          None
        }
      }
    }
  },
  "push" -> MBuiltinFunction { args =>
    argSizeCheck(2, args) {
      arrayCheck("push", _) { (array, _) =>
        Some(MArray(array.elements.appended(args(1))))
      }
    }
  }
)

extension (l: List[(String, MBuiltinFunction)]) {
  def getBuiltinByName(name: String): Option[MBuiltinFunction] = {
    l.find { case (functionName, _) =>
      name == functionName
    }.map(_._2)
  }
}