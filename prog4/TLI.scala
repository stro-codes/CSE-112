// CSE 112 Programming Assignment 4 by Strother Woog - 1618221 @ University of California, Santa Cruz

import com.sun.org.apache.xerces.internal.util.SymbolTable

import scala.collection.mutable.{ListBuffer, Map}
import scala.io.Source
import scala.language.postfixOps
import scala.util.control.Exception
import java.io.IOException

import com.sun.net.httpserver.Authenticator.Success
import scala.util.{Try, Success, Failure}
//import scala.util.Try


abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: ListBuffer[Expr]) extends Stmt

object TLI {
  def eval(expr: Expr, symTab: Map[String, Double]): Double = expr match {

      case BinOp ("+", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else eval (e1, symTab) + eval (e2, symTab)
      }
      case BinOp ("-", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else eval (e1, symTab) - eval (e2, symTab)
      }
      case BinOp ("*", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else eval (e1, symTab) * eval (e2, symTab)
      }
      case BinOp ("/", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else eval (e1, symTab) / eval (e2, symTab)
      }
      case BinOp (">", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else {
          if (eval(e1, symTab) > eval(e2, symTab)) 1 else 0
        }
      }
      case BinOp ("<", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else {
          if (eval(e1, symTab) < eval(e2, symTab)) 1 else 0
        }
      }
      case BinOp (">=", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else {
          if (eval(e1, symTab) >= eval(e2, symTab)) 1 else 0
        }
      }
      case BinOp ("<=", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else {
          if (eval(e1, symTab) <= eval(e2, symTab)) 1 else 0
        }
      }
      case BinOp ("==", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else {
          if (eval(e1, symTab) == eval(e2, symTab)) 1 else 0
        }
      }
      case BinOp ("!=", e1, e2) => {
        if (eval(e1, symTab) == 0.00000000001 || eval(e2, symTab) == 0.00000000001) 0.00000000001
        else {
          if (eval(e1, symTab) != eval(e2, symTab)) 1 else 0
        }
      }

      case Var (name) => {
        if (symTab.contains(name)) symTab(name)
        else 0.00000000001
      }
      case Constant (num) => num

      case _ => 0.00000000001 // should really throw an error

  }

  def evalStr(expr: Expr): String = expr match {
    case Str(name) => name.substring(1, name.length - 1)
  }

  def perform(stmt: Stmt, symTab: Map[String, Double]) : (Int, Map[String, Double]) = stmt match {
    case Let(str, expr) => {
      if (expr.isInstanceOf[Var]) {
        if (eval(expr, symTab)== 0.00000000001) {
          print("Undefined variable " + str + " at line ")
          return (-3, symTab)
        }
      }
      symTab.addOne(str, eval(expr, symTab))
      (-1, symTab)
    }
    case Print(exprList) => {
      for (expr <- exprList) {
        if (expr.isInstanceOf[Str]) print(evalStr(expr) + " ")
        else if (expr.isInstanceOf[Var]) {
          if (eval(expr, symTab)== 0.00000000001) {
            print("Undefined variable " + expr + " at line ")
            return (-3, symTab)
          }
          else print(eval(expr, symTab) + " ")
        }
        else print(eval(expr, symTab) + " ")

      }
      println()
      (-1, symTab)
    }
    case Input(str) => {
      try {
        val in = scala.io.StdIn.readDouble()
        val expr = new Constant(in)
        symTab.addOne(str, eval(expr, symTab))
        (-1, symTab)
      } catch {
        case _ : Exception => {
          println("Illegal or missing input")
          System.exit(0)
          (-3, symTab)
        }
      }
    }
    case If(expr, label) => {
      val cond = eval(expr, symTab)
      if (cond == 1) {
        if (symTab.contains(label)) (symTab(label).toInt, symTab)
        else {
          print("Illegal goto " + label + " at line ")
          (-2, symTab)
        }
      }
      else {
        (-1, symTab)
      }
    }
    case _ => (0, symTab) // potential error placement
  }

  def isDouble(str: String) : Int = {
    if(str.contains('.')) {
      val newStr = str.replaceAll(".", "")
      if(newStr.forall(_.isDigit)) 1
      else 0
    }
    else 0
  }

  def main(args: Array[String]) {
    // Symbol Table
    var symTable : Map[String, Double] = Map()

    // File Handling
    val fileName = args(0)
    val file = Source.fromFile(fileName)
    var fileLines = new ListBuffer[String]()

    for (line <- file.getLines()) {
      fileLines += line
    }

    file.close()

    // Line Parsing (Remove Whitespaces & Blank Lines)
    var parsedLines = new ListBuffer[ListBuffer[String]]()

    for (line <- fileLines) {
      var parseLine = new ListBuffer[String]()
      val spacesRemoved = line.split("\\s+(?=(?:[^\\\"]*\\\"[^\\\"]*\\\")*[^\\\"]*$)")

      for (pl <- spacesRemoved)
        if (pl != "") parseLine += pl

      if (parseLine != ListBuffer()) parsedLines += parseLine
    }

    // Add Labels to SymTable
    var i = 1
    for (line <- parsedLines) {
      if (line(0).takeRight(1) == ":") {
        symTable.addOne(line(0).dropRight(1), i.toDouble)
      }
      i += 1
    }

    // Create a List Buffer of Statements
    i = 1
    var stmtList = new ListBuffer[Stmt]
    for (l <- parsedLines) {
      var line = l
      var key = line(0)
      if (key.last == ':') {
        key = line(1)
        line.remove(0)
      }

      if (key == "let") {
        if (line.length == 4) {
          if (isDouble(line(3)) == 1) stmtList += new Let(line(1), Constant(line(3).toDouble))
          else if (line(3).forall(_.isDigit)) stmtList += new Let(line(1), Constant(line(3).toDouble))
          else stmtList += new Let(line(1), Var(line(3)))
        }
        else if (line.length == 6) {
          var e = new ListBuffer[Expr]

          if (isDouble(line(3)) == 1) e += Constant(line(3).toDouble)
          else if (line(3).forall(_.isDigit)) e += Constant(line(3).toDouble)
          else e += Var(line(3))

          if (isDouble(line(5)) == 1) e += Constant(line(5).toDouble)
          else if (line(5).forall(_.isDigit)) e += Constant(line(5).toDouble)
          else e += Var(line(5))

          if (line(4) != "+" && line(4) != "-" && line(4) != "*" && line(4) != "/" && line(4) != "==" && line(4) != "!=" && line(4) != ">" && line(4) != "<" && line(4) != ">=" && line(4) != "<=") {
            println("Syntax error on line " + i + ".")
            System.exit(0)
          }

          stmtList += new Let(line(1), BinOp(line(4), e(0), e(1)))
        }
        /*else if (line.length > 6 && line.length % 2 == 0) {
          var e = new ListBuffer[Expr]
          var e1 = 0.0
          var o = 4
          var e2 = 5

          if (isDouble(line(3)) == 1) e1 = eval(Constant(line(3).toDouble), symTable)
          else if (line(3).forall(_.isDigit)) e1 = eval(Constant(line(3).toDouble), symTable)
          else e1 = eval(Var(line(3)), symTable)

          println("step 1")

          while (e2 < line.length - 1) {

            if (line(o) != "+" && line(o) != "-" && line(o) != "*" && line(o) != "/" && line(o) != "==" && line(o) != "!=" && line(o) != ">" && line(o) != "<" && line(o) != ">=" && line(o) != "<=") {
              println("Syntax error on line " + i + ".")
              System.exit(0)
            }

            if (isDouble(line(e2)) == 1) e1 = eval(Constant(line(e2).toDouble), symTable)
            else if (line(e2).forall(_.isDigit)) e1 = eval(Constant(line(e2).toDouble), symTable)
            else e1 = eval(Var(line(e2)), symTable)

            o += 2
            e2 += 2
          }

          e += Constant(e1)
          if (isDouble(line(line.length - 1)) == 1) e += Constant(line(line.length - 1).toDouble)
          else if (line(line.length - 1).forall(_.isDigit)) e += Constant(line(line.length - 1).toDouble)
          else e += Var(line(line.length - 1))

          stmtList += new Let(line(1), BinOp(line(line.length - 2), e(0), e(1)))
        } */
        else {

        }
      }
      else if (key == "print") {
        var exprList = new ListBuffer[Expr]
        var e = new ListBuffer[String]
        line.remove(0)

        for (p <- line) {
          if (p != ",") e += p
          else {
            if (e.length == 1) {

              if (isDouble(e(0)) == 1) exprList += Constant(e(0).toDouble)
              else if (e(0).forall(_.isDigit)) exprList += Constant(e(0).toDouble)
              else if (e(0).charAt(0) == '"') exprList += Str(e(0))
              else exprList += Var(e(0))

            }
            else if (e.length == 3) {
              var binExpr = new ListBuffer[Expr]

              if (isDouble(e(0)) == 1) binExpr += Constant(e(0).toDouble)
              else if (e(0).forall(_.isDigit)) binExpr += Constant(e(0).toDouble)
              else binExpr += Var(e(0))

              if (isDouble(e(2)) == 1) binExpr += Constant(e(2).toDouble)
              else if (e(2).forall(_.isDigit)) binExpr += Constant(e(2).toDouble)
              else binExpr += Var(e(2))

              if (e(1) != "+" && e(1) != "-" && e(1) != "*" && e(1) != "/" && e(1) != "==" && e(1) != "!=" && e(1) != ">" && e(1) != "<" && e(1) != ">=" && e(1) != "<=") {
                println("Syntax error on line " + i + ".")
                System.exit(0)
              }

              exprList += BinOp(e(1), binExpr(0), binExpr(1))
            }

            e = new ListBuffer[String]
          }
        }
        if (e.length == 1) {

          if (isDouble(e(0)) == 1) exprList += Constant(e(0).toDouble)
          else if (e(0).forall(_.isDigit)) exprList += Constant(e(0).toDouble)
          else if (e(0).charAt(0) == '"') exprList += Str(e(0))
          else exprList += Var(e(0))

        }
        else if (e.length == 3) {
          var binExpr = new ListBuffer[Expr]

          if (isDouble(e(0)) == 1) binExpr += Constant(e(0).toDouble)
          else if (e(0).forall(_.isDigit)) binExpr += Constant(e(0).toDouble)
          else binExpr += Var(e(0))

          if (isDouble(e(2)) == 1) binExpr += Constant(e(2).toDouble)
          else if (e(2).forall(_.isDigit)) binExpr += Constant(e(2).toDouble)
          else binExpr += Var(e(2))

          exprList += BinOp(e(1), binExpr(0), binExpr(1))
        }

        stmtList += new Print(exprList)
      }
      else if (key == "input") {
        stmtList += new Input(line(1))
      }
      else if (key == "if") {
        var e = new ListBuffer[Expr]

        if (isDouble(line(1)) == 1) e += Constant(line(1).toDouble)
        else if (line(1).forall(_.isDigit)) e += Constant(line(1).toDouble)
        else e += Var(line(1))

        if (isDouble(line(3)) == 1) e += Constant(line(3).toDouble)
        else if (line(3).forall(_.isDigit)) e += Constant(line(3).toDouble)
        else e += Var(line(3))

        val expr = BinOp(line(2), e(0), e(1))
        stmtList += new If(expr, line.last)
      }
      else {
        println("Syntax error on line " + i + ".")
        System.exit(0)
      }
      i += 1
    }

    // Iterate through List Buffer of Statements
    i = 0
    while (i != stmtList.length) {
      val performLine = perform(stmtList(i), symTable)
      symTable = performLine._2
      if (performLine._1 > -1) {
        i = performLine._1 - 2
      }
      else if (performLine._1 == -2) {
        println((i+1) + ".")
        System.exit(0)
      }
      else if (performLine._1 == -3) {
        println((i+1) + ".")
        System.exit(0)
      }
      i += 1
    }

  }
}