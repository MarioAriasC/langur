package org.marioarias.langur.objects

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
    obj.isInstanceOf[MBoolean] == value
  }
}

object MNull extends MObject {
  override def inspect(): String = "null"
}

extension (m: Option[MObject]) {
  def typeDesc(): String = m match {
    case Some(v) => m.typeDesc()
    case None => "null"
  }
}

extension (m: MObject) {
  def typeDesc(): String = m.getClass.getSimpleName
}