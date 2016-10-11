package com.fnplatypus
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.JavaTokenParsers

class LoopParser extends RegexParsers with JavaTokenParsers {
  override type Elem = Char
  def identifier  = """[_\p{L}][_\p{L}\p{Nd}]*""".r
  def func        =
    "fn" ~identifier~"("~arguments~")"~"->"~block ^^
      { case f~func_name~pi~arguments~pe~ar~body => FunctionDef(func_name, arguments, body) }

  def letval        =
    "let" ~identifier~"="~expr~"in"~block ^^
      { case l~iden~eq~value~in~body => LetVal(iden, value, body) }
  def print =
    "print" ~identifier ^^
      {case p~iden => PrintStatement(iden)}

  def func_apply =
    identifier ~ "(" ~ arguments ~ ")" ^^
      {case func_name ~ lp ~ argz ~ rp  => FuncApply(func_name, argz)}
  def integer: Parser[Num]     = """(0|[1-9]\d*)""".r ^^ { value: String => Num(value.toInt) }
  def argument = identifier
  def arguments = argument*
  def statements = statement*
  def block = "{"~>statements<~"}"  ^^ { l => Block(l) }


  lazy val expr: Parser[Tree] = term ~ rep("[+-]".r ~ term) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "+" ~ t2) => Add(t1, t2)
      case (t1, "-" ~ t2) => Sub(t1, t2)
    }
  }

  lazy val term = factor ~ rep("[*/]".r ~ factor) ^^ {
    case t ~ ts => ts.foldLeft(t) {
      case (t1, "*" ~ t2) => Mul(t1, t2)
      case (t1, "/" ~ t2) => Div(t1, t2)
    }
  }

  lazy val factor = "(" ~> expr <~ ")" | num | func_apply

  lazy val num = floatingPointNumber ^^ { t => Num(t.toDouble) }

  def statement : Parser[Statement] =  block | func | letval | print  | func_apply| expr



}

abstract trait Statement
case class Block(statements : List[Statement]) extends Statement
case class FunctionDef(name: String, arguments: List[String],  body:Statement) extends Statement
case class LetVal(name: String, value: Tree,  body:Statement) extends Statement
case class PrintStatement(name: String) extends Statement

case class Context(values: Map[String, Any], functions: Map[String, FunctionDef], returnValue: Either[Num, NullValue])

sealed abstract class Tree extends Statement
case class FuncApply(name: String, arguments: List[Any]) extends Tree
case class Add(t1: Tree, t2: Tree) extends Tree
case class Sub(t1: Tree, t2: Tree) extends Tree
case class Mul(t1: Tree, t2: Tree) extends Tree
case class Div(t1: Tree, t2: Tree) extends Tree
case class Num(t: Double) extends Tree

case class NullValue() extends Tree

class TestSimpleParser extends LoopParser  {
  def main() = {
    //evaluate(parseAll(statements, "fn foo(a) -> {print a } let x = 2 in { foo(x) let x = 1 in {print x}} let x = 1 in { foo(x)}"))
    evaluate(parseAll(statements, "fn foo() -> { 3 }let x = (5 + foo()) in {print x} "))
    }
  val startingReturnValue: Either[Num, NullValue] = Right(NullValue())
  val startingContext: Context = Context(Map[String, Any](), Map[String,FunctionDef](), startingReturnValue )
  def evaluate(parseResult: ParseResult[List[Statement]]): Unit = {
    parseResult match {
      case Success(ast, _) => ast.foldLeft(startingContext){(z, i) => eval(z, i)}
      case x => {
        println("Failure to parse:")
        println(x)
      }
    }
  }

  def  treeval(t: Tree)(implicit context: Context): Num = t match {
    case Add(t1, t2) => PlattyAdd(treeval(t1), treeval(t2))
    case Sub(t1, t2) => PlattySub(treeval(t1),treeval(t2))
    case Mul(t1, t2) => PlattyMul(treeval(t1),treeval(t2))
    case Div(t1, t2) => PlattyDiv(treeval(t1),treeval(t2))
    case fn @ FuncApply(name, arguments) =>
      callFunction(context, fn)
    case num @ Num(t) => num
  }

  def PlattyAdd(val1: Num, val2: Num): Num = {
    Num(val1.t + val2.t)

  }

  def PlattySub(val1: Num, val2: Num): Num = {

    Num(val1.t - val2.t)
  }

  def PlattyMul(val1: Num, val2: Num): Num = {

    Num(val1.t * val2.t)
  }

  def PlattyDiv(val1: Num, val2: Num): Num = {

    Num(val1.t / val2.t)
  }

  def callFunction(context: Context, func: FuncApply): Num  = {
    val functionToApply = context.functions(func.name)
    val argumentValues = func.arguments.map{case a: String => context.values(a)}
    val newContextValues = context.values ++ functionToApply.arguments.zip(argumentValues)
    val newContext = Context(newContextValues, context.functions, Right(NullValue()))
    eval(newContext, functionToApply.body).returnValue match {
      case Left(value) =>
        value
      case Right(value) =>
        Num(0)

    }
  }

  def eval(context: Context, stat: Statement): Context = {

    implicit val ctx: Context = context
    stat match {
      case Block(statements) => statements.foldLeft(context){ (z, i) => eval(z, i)}
      case FunctionDef(name, arguments, body) =>  {
        val newFunctions = context.functions + (name -> FunctionDef(name, arguments, body))
        val newContext   = Context(context.values, newFunctions, Right(NullValue()))
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

      case tree: Tree =>
        context.copy(returnValue = Left(treeval(tree)))

      case fn @ FuncApply(name, arguments) => {
        val returnValue = callFunction(context, fn)
        context.copy(returnValue=Left(returnValue))
      }
      case LetVal(identifier, value, body) =>  {
            val newVal =treeval(value)
            val newValues = context.values + (identifier -> newVal)
            val newContext = context.copy(values = newValues, returnValue = Left(newVal))
            eval(newContext, body)
      }

    }

  }

}


object Main extends App  {
  val parser = new TestSimpleParser()
  parser.main
}

