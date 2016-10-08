package com.fnplatypus
import scala.util.parsing.combinator.RegexParsers

class LoopParser extends RegexParsers {
  override type Elem = Char
  def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def func        =
    "fn" ~identifier~"("~arguments~")"~"->"~block ^^
      { case f~func_name~pi~arguments~pe~ar~body => FunctionDef(func_name, arguments, body) }

  def letval        =
    "let" ~identifier~"="~integer~"in"~block ^^
      { case l~iden~eq~value~in~body => LetVal(iden, value, body) }
  def print =
    "print" ~identifier ^^
      {case p~iden => PrintStatement(iden)}

  def func_apply =
    identifier ~ "(" ~ arguments ~ ")" ^^
      {case func_name ~ lp ~ argz ~ rp  => FuncApply(func_name, argz)}
  def integer     = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def argument = identifier
  def arguments = argument*
  def statements = statement*
  def block = "{"~>statements<~"}"  ^^ { l => Block(l) }
  def statement : Parser[Statement] =  block | func | letval | print  | func_apply
}

abstract trait Statement
case class Block(statements : List[Statement]) extends Statement
case class FunctionDef(name: String, arguments: List[String],  body:Statement) extends Statement
case class LetVal(name: String, value: Integer,  body:Statement) extends Statement
case class PrintStatement(name: String) extends Statement
case class FuncApply(name: String, arguments: List[Any]) extends Statement


case class Context(values: Map[String, Any], functions: Map[String, FunctionDef])

class TestSimpleParser extends LoopParser {
  def main() = evaluate(parseAll(statements, "fn foo(a) -> {print a } let x = 2 in { foo(x) let x = 1 in {print x}} let x = 1 in { foo(x)}"))
  val startingContext: Context = Context(Map[String, Any](), Map[String,FunctionDef]() )
  def evaluate(parseResult: ParseResult[List[Statement]]): Unit = {
    parseResult match {
      case Success(ast, _) => ast.foldLeft(startingContext){(z, i) => eval(z, i)}
      case x => {
        println("Failure to parse:")
        println(x)
      }
    }
  }

  def eval(context: Context, stat: Statement): Context = {

    stat match {
      case Block(statements) => statements.foldLeft(context){ (z, i) => eval(z, i)}
      case FunctionDef(name, arguments, body) =>  {
        val newFunctions = context.functions + (name -> FunctionDef(name, arguments, body))
        val newContext   = Context(context.values, newFunctions)
        return newContext
      }
      case PrintStatement(iden) =>  {
        val value = context.values get iden
        value match {
          case Some(concrete_value) =>
            println(concrete_value)
          case None =>
            println("Value not found")
        }
        context
      }

      case FuncApply(name, arguments) => {
        val functionToApply = context.functions(name)
        val argumentValues = arguments.map{case a: String => context.values(a)}
        println(functionToApply.arguments.zip(argumentValues))
        val newContextValues = context.values ++ functionToApply.arguments.zip(argumentValues)
        val newContext = Context(newContextValues, context.functions)
        eval(newContext, functionToApply.body)
      }
      case LetVal(identifier, value, body) =>  {
        val newValues = context.values + (identifier -> value)
        val newContext = Context(newValues, context.functions)
        eval(newContext, body)
      }
    }
  }
}


object Main extends App  {
  val parser = new TestSimpleParser()
  parser.main
}

