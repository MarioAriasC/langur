package org.marioarias.langur.evaluator

import org.marioarias.langur.ast.*
import org.marioarias.langur.objects.*
import org.marioarias.langur.utils.Utils.also

import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 *
 * @author Mario Arias
 *         Date: 19/2/22
 *         Time: 5:19 PM
 */
object Evaluator {
  val NULL: MNull.type = MNull
  val TRUE: MBoolean = MBoolean(true)
  val FALSE: MBoolean = MBoolean(false)

  extension (b: Boolean) {
    def toMonkey: MBoolean = if (b) TRUE else FALSE
  }

  def eval(node: Option[Node], env: Environment): Option[MObject] = {
    node.flatMap {
      case p: Program => evalProgram(p.statements, env)
      case e: ExpressionStatement => eval(e.expression, env)
      case i: IntegerLiteral => Some(MInteger(i.value))
      case p: PrefixExpression => eval(p.right, env).ifNotError { right =>
        Some(evalPrefixExpression(p.operator, right))
      }
      case i: InfixExpression => eval(i.left, env).ifNotError { left =>
        eval(i.right, env).ifNotError { right =>
          Some(evalInfixExpression(i.operator, left, right))
        }
      }
      case b: BooleanLiteral => Some(b.value.toMonkey)
      case i: IfExpression => evalIfExpression(i, env)
      case b: BlockStatement => evalBlockStatement(b, env)
      case r: ReturnStatement => eval(r.returnValue, env).ifNotError(value => Some(MReturnValue(value)))
      case l: LetStatement => eval(l.value, env).ifNotError(value => Some(env.put(l.name.value, value)))
      case f: FunctionLiteral => Some(MFunction(f.parameters, f.body, env))
      case c: CallExpression =>
        eval(c.function, env).ifNotError { function =>
          val args = evalExpressions(c.arguments, env)
          if (args.size == 1 && args.head.isError) {
            args.head
          } else {
            applyFunction(function, args)
          }
        }
      case i: Identifier => Some(evalIdentifier(i, env))
      case s: StringLiteral => Some(MString(s.value))
      case i: IndexExpression =>
        val left = eval(i.left, env)
        if (left.isError) {
          return left
        }

        val index = eval(i.index, env)
        if (index.isError) {
          return index
        }
        evalIndexExpression(left, index)
      case h: HashLiteral => evalHashLiteral(h, env)
      case a: ArrayLiteral =>
        val elements = evalExpressions(a.elements, env)
        if (elements.size == 1 && elements.head.isError) {
          elements.head
        } else {
          Some(MArray(elements))
        }
      //      case _ => None
    }
  }

  private def evalProgram(statements: List[Statement], env: Environment): Option[MObject] = {
    var result: Option[MObject] = None
    statements.foreach { statement =>
      result = eval(Some(statement), env)

      result.foreach {
        case r: MReturnValue => return Some(r.value)
        case e: MError => return Some(e)
        case _ => //Nothing
      }
    }
    result
  }

  private def evalPrefixExpression(operator: String, right: MObject): MObject = {
    operator match {
      case "!" => evalBangOperatorExpression(right)
      case "-" => evalMinusPrefixOperatorExpression(right)
      case _ => MError(s"Unknown operator: $operator${right.typeDesc()}")
    }
  }

  private def evalMinusPrefixOperatorExpression(right: MObject): MObject = {
    right match {
      case i: MInteger => -i
      case _ => MError(s"unknown operator: -${right.typeDesc()}")
    }
  }

  private def evalBangOperatorExpression(right: MObject): MObject = {
    right match {
      case TRUE => FALSE
      case FALSE => TRUE
      case NULL => TRUE
      case _ => FALSE
    }
  }

  private def evalInfixExpression(operator: String, left: MObject, right: MObject): MObject = {
    (left, operator, right) match {
      case (left: MInteger, "+", right: MInteger) => left + right
      case (left: MInteger, "-", right: MInteger) => left - right
      case (left: MInteger, "*", right: MInteger) => left * right
      case (left: MInteger, "/", right: MInteger) => left / right
      case (left: MInteger, "<", right: MInteger) => (left < right).toMonkey
      case (left: MInteger, ">", right: MInteger) => (left > right).toMonkey
      case (left: MInteger, "==", right: MInteger) => (left == right).toMonkey
      case (left: MInteger, "!=", right: MInteger) => (left != right).toMonkey
      case (_, "==", _) => (left == right).toMonkey
      case (_, "!=", _) => (left != right).toMonkey
      case (_, _, _) if left.typeDesc() != right.typeDesc() => MError(s"type mismatch: ${left.typeDesc()} $operator ${right.typeDesc()}")
      case (left: MString, "+", right: MString) => left + right
      case (_, _, _) => MError(s"unknown operator: ${left.typeDesc()} $operator ${right.typeDesc()}")
    }
  }

  private def evalIfExpression(expression: IfExpression, env: Environment): Option[MObject] = {
    def isTruthy(obj: MObject): Boolean = obj match {
      case NULL => false
      case TRUE => true
      case FALSE => false
      case _ => true
    }

    eval(expression.condition, env).ifNotError { condition =>
      if (isTruthy(condition)) {
        eval(expression.consequence, env)
      } else if (expression.alternative.isDefined) {
        eval(expression.alternative, env)
      } else {
        Some(NULL)
      }
    }
  }

  private def evalBlockStatement(block: BlockStatement, env: Environment): Option[MObject] = {
    var result: Option[MObject] = None
    for statements <- block.statements yield {
      statements.foreach { statement =>
        result = eval(statement, env)
        result.map { some =>
          if (some.isInstanceOf[MReturnValue] || some.isInstanceOf[MError]) {
            return result
          }
        }
      }
    }
    result
  }

  extension (o: Option[MObject]) {
    def isError: Boolean = o match {
      case Some(e: MError) => true
      case Some(_) => false
      case None => false
    }
  }

  private def evalExpressions(arguments: Option[List[Option[Expression]]], env: Environment): List[Option[MObject]] = {
    arguments.map(_.map { argument =>
      val evaluated = eval(argument, env)
      if (evaluated.isError) {
        return List(evaluated)
      }
      evaluated
    }).getOrElse(List.empty)
  }

  private def applyFunction(function: MObject, args: List[Option[MObject]]): Option[MObject] = {
    function match {
      case f: MFunction =>
        val extendEnv = extendFunctionEnv(f, args)
        val evaluated = eval(f.body, extendEnv)
        unwrapReturnValue(evaluated)
      case f: MBuiltinFunction => f.fn(args).orElse(Some(NULL))
      case _ => Some(MError(s"not a function: ${function.typeDesc()}"))
    }
  }

  private def unwrapReturnValue(maybeObject: Option[MObject]): Option[MObject] = {
    maybeObject match {
      case Some(r: MReturnValue) => Some(r.value)
      case _ => maybeObject
    }
  }

  private def extendFunctionEnv(function: MFunction, args: List[Option[MObject]]): Environment = {
    val env = Environment.newEnclosedEnvironment(function.env)
    function.parameters.foreach { parameters =>
      parameters.zipWithIndex.foreach { case (identifier, i) =>
        env(identifier.value) = args(i).get
      }
    }
    env
  }

  private def evalIdentifier(node: Identifier, env: Environment): MObject = {
    val value = env(node.value)
    value match {
      case Some(v) => v
      case None =>
        Builtins(node.value) match {
          case Some(b: MBuiltinFunction) => b
          case None => MError(s"identifier not found: ${node.value}")
        }
    }
  }

  private def evalIndexExpression(left: Option[MObject], index: Option[MObject]): Option[MObject] = {
    (left, index) match {
      case (Some(array: MArray), Some(i: MInteger)) => evalArrayIndexExpression(array, i)
      case (Some(hash: MHash), _) => evalHashIndexExpression(hash, index)
      case (_, _) => Some(MError(s"index operator not supported: ${left.typeDesc()}"))
    }
  }

  private def evalArrayIndexExpression(array: MArray, index: MInteger): Option[MObject] = {
    val elements = array.elements
    val i = index.value
    val max = elements.size - 1

    if (i < 0 || i > max) {
      return Some(NULL)
    }

    elements(i.toInt)
  }

  private def evalHashLiteral(hash: HashLiteral, env: Environment): Option[MObject] = {
    val pairs = mutable.HashMap.empty[HashKey, HashPair]
    hash.pairs.foreach { (keyNode, valueNode) =>
      val key = eval(Some(keyNode), env)
      if (key.isError) {
        return key
      }
      key match {
        case Some(hashable: MHashable[?]) =>
          val value = eval(Some(valueNode), env)
          if (value.isError) {
            return value
          }
          pairs(hashable.hashKey()) = HashPair(hashable, value.get)
        case _ => return Some(MError(s"unusable as a hash key: ${key.typeDesc()}"))
      }
    }
    Some(MHash(pairs.toMap))
  }

  private def evalHashIndexExpression(hash: MHash, index: Option[MObject]): Option[MObject] = {
    index match {
      case Some(h: MHashable[?]) =>
        val pair = hash.pairs.get(h.hashKey())
        pair match {
          case Some(value: HashPair) => Some(value.value)
          case None => Some(NULL)
        }
      case _ =>
        Some(MError(s"unusable as a hash key: ${index.typeDesc()}"))
    }
  }

  extension (e: Option[MObject]) {
    def ifNotError(body: MObject => Option[MObject]): Option[MObject] = {
      e.flatMap {
        case me: MError => e
        case ne: _ => body(ne)
      }
    }
  }
}
