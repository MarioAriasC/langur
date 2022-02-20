package org.marioarias.langur.evaluator

import org.marioarias.langur.ast.*
import org.marioarias.langur.objects.*

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
    if (!right.isInstanceOf[MInteger]) {
      MError(s"unknown operator: -${right.typeDesc()}")
    } else {
      -right.asInstanceOf[MInteger]
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
    if (left.isInstanceOf[MInteger] && right.isInstanceOf[MInteger]) {
      evalIntegerInfixExpression(operator, left.asInstanceOf[MInteger], right.asInstanceOf[MInteger])
    } else if (operator == "==") {
      (left == right).toMonkey
    } else if (operator == "!=") {
      (left != right).toMonkey
    } else if (left.typeDesc() != right.typeDesc()) {
      MError(s"type mismatch: ${left.typeDesc()} $operator ${right.typeDesc()}")
    } else {
      MError(s"unknown operator: ${left.typeDesc()} $operator ${right.typeDesc()}")
    }
  }

  private def evalIntegerInfixExpression(operator: String, left: MInteger, right: MInteger): MObject = {
    operator match {
      case "+" => left + right
      case "-" => left - right
      case "*" => left * right
      case "/" => left / right
      case "<" => (left < right).toMonkey
      case ">" => (left > right).toMonkey
      case "==" => (left == right).toMonkey
      case "!=" => (left != right).toMonkey
      case _ => MError(s"unknown operator: ${left.typeDesc()} $operator ${right.typeDesc()}")
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

  extension (e: Option[MObject]) {
    def ifNotError(body: MObject => Option[MObject]): Option[MObject] = {
      e.flatMap {
        case me: MError => e
        case ne: _ => body(ne)
      }
    }
  }
}
