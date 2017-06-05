/*
   Copyright 2013 Ilya Lakhin (Илья Александрович Лахин)

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package name.lakhin.eliah.projects.papacarlo.js.demo

import scala.scalajs.js
import js.JSConverters._
import js.annotation.{JSExport, JSName}
import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import name.lakhin.eliah.projects.papacarlo.syntax.Node
import kz.greetgo.dtlang.DtLang

import scala.scalajs.js.Dynamic

//import name.lakhin.eliah.projects.papacarlo.examples.Json

import scala.scalajs.js.UndefOr

/**
  * Created by den on 20.06.16.
  */
@JSExport("DtLangParser")
class DtLangParser {
  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)
  private var lastAddedNode = Option.empty[Int]
  private var lastRemovedNode = Option.empty[Int]
  private var callback: UndefOr[js.Dynamic] = js.undefined

  // TODO remove
  private var addedNodes = List.empty[Int]
  private var removedNodes = List.empty[Int]
  syntax.onNodeCreate.bind { node => addedNodes ::= node.getId }
  syntax.onNodeRemove.bind { node => removedNodes ::= node.getId }

  val statements = List(
    "empty", "assign", "group",
    "condition", "case",
    "foreach", "break", "continue",
    "procedure", "exit", "stop", "message", "error"
  )

  @JSExport
  def input(text: String) = {
    lexer.input(text)
    if (syntax.getErrors.isEmpty) try {
      val stats: Option[Dynamic] = extractStats(syntax.getRootNode.get)
      if (!stats.isEmpty) callback.get(stats.get) else callback.get(js.Dynamic.literal())
    } catch {
      case e: StructureException => callback.get(js.Dynamic.literal("errors" -> e.dynamic))
    }
    else callback.get(js.Dynamic.literal("errors" -> getErrors()))
  }

  def extractStats(node: Node, parent: Option[Int] = None): Option[js.Dynamic] = {
    for (
      result <- node.getBranches.getOrElse("result", List.empty);
      call <- result.getBranches.getOrElse("call", List.empty);
      path <- result.getBranches.getOrElse("path", List.empty);
      segment <- path.getBranches.getOrElse("segment", List.empty);
      name <- segment.getValues.getOrElse("name", List.empty);
      if (statements contains name)
    ) try {
      val id = node.getId
      val e = (i: Int) => call.getBranches("expr")(i).sourceCode
      val element = js.Dynamic.literal(
        "id" -> id,
        "parent" -> parent.map(_.toString).getOrElse[String]("#"),
        "text" -> (name match {
          case "assign" => {
            e(0) + " := " + e(1)
          }
          case "case" => {
            e(0)
          }
          case "foreach" => {
            "foreach " + e(0) + " := " + e(1) + " .. " + e(2)
          }
          case "break" => {
            val exprs = call.getBranches.get("expr")
            if (exprs.isEmpty) "break" else "break " + exprs.get(0).sourceCode
          }
          case "continue" => {
            val exprs = call.getBranches.get("expr")
            if (exprs.isEmpty) "continue" else "continue " + exprs.get(0).sourceCode
          }
          case "procedure" => {
            "procedure " + e(0)
          }
          case "message" => {
            "message " + e(0)
          }
          case "error" => {
            "error " + e(0)
          }
          case "group" => {
            val comment = call.getBranches("comment")(0)
            if (comment.sourceCode.length == 0) "group" else "group " + comment.sourceCode
          }
          case _ => name
        }),
        "type" -> name
      )
      val children = new js.Array[js.Any]
      if (name != "assign")
        for (subExpr <- call.getBranches.getOrElse("expr", List.empty)) {
          val child = extractStats(subExpr, Some(id))
          child.foreach(children.push(_))
        }
      if (children.length > 0)
        element.updateDynamic("children")(children)
      name match {
        case "group" =>
          element.updateDynamic("commentId")(call.getBranches("comment")(0).getId)
        case "case" =>
          element.updateDynamic("predicateId")(call.getBranches("expr")(0).getId)
          element.updateDynamic("predicate")(e(0))
        case "assign" =>
          element.updateDynamic("assignTo")(e(0))
          element.updateDynamic("expression")(e(1))
        case "error" =>
          element.updateDynamic("value")(e(0))
        case "foreach" =>
          element.updateDynamic("variable")(e(0))
          element.updateDynamic("start")(e(1))
          element.updateDynamic("finish")(e(2))
          element.updateDynamic("to")(tokenCursor(call.getBranches("expr")(2).getEnd, after = true))
        case "message" =>
          element.updateDynamic("value")(e(0))
        case "procedure" =>
          element.updateDynamic("name")(e(0))
        case _ =>
      }
      return Some(element)
    } catch {
      case e: IndexOutOfBoundsException =>
        val from = tokenCursor(node.getBegin)
        val to = tokenCursor(node.getEnd, after = true)

        throw new StructureException(js.Dynamic.literal(
          "from" -> from,
          "to" -> to,
          "description" -> "Invalid structure"
        ))
    }
    None
  }

  @JSExport
  def onChange(callback: js.Dynamic) = {
    this.callback = callback
  }

  private def tokenPos(token: TokenReference, after: Boolean = false) = {
    val c = token.collection.cursor(token.index + (if (after) 1 else 0))
    (c._1 - 1, c._2 - 1)
  }

  // for demo

  //@JSExport
  def inputAll(text: String) {
    lexer.input(text)
  }

  //@JSExport
  def input(text: String,
            fromLine: Int,
            fromChar: Int,
            toLine: Int,
            toChar: Int) {
    lexer.input(text, fromLine -> fromChar, toLine -> toChar)
  }

  //@JSExport
  def getErrors() = {
    toJsArray(syntax.getErrors.map {
      error =>
        val from = tokenCursor(error.from)
        val to = tokenCursor(error.to, after = true)

        js.Dynamic.literal(
          "from" -> from,
          "to" -> to,
          "description" -> error.description
        )
    })
  }

  @JSExport
  def getNodeFragment(id: Int) = {
    syntax.nodes.get(id) match {
      case Some(node) =>
        js.Dynamic.literal(
          "exists" -> true,
          "id" -> id,
          "from" -> tokenCursor(node.getBegin),
          "to" -> tokenCursor(node.getEnd, after = true)
        )

      case None => js.Dynamic.literal(
        "exists" -> false,
        "id" -> id
      )
    }
  }

  @JSExport
  def getNodeSource(id: Int) = {
    syntax.nodes.get(id) match {
      case Some(node) =>
        js.Dynamic.literal(
          "exists" -> true,
          "id" -> id,
          "source" -> node.sourceCode
        )

      case None => js.Dynamic.literal(
        "exists" -> false,
        "id" -> id
      )
    }
  }

  @JSExport
  def getAST(graph: Boolean = false) = {
    val result = js.Dictionary.empty[js.Any]

    result("total") = syntax.nodes.size
    result("added") = toJsArray(addedNodes.reverse.map(x => x: js.Any))
    result("removed") = toJsArray(removedNodes.reverse.map(x => x: js.Any))

    if (graph) {
      val ast = js.Dictionary.empty[js.Any]

      for (node <- syntax.nodes.elements) {
        ast(node.getId.toString) = exportNode(node)
      }

      result("all") = ast
    }

    addedNodes = Nil
    removedNodes = Nil

    result
  }

  private def toJsArray(iterable: Iterable[js.Any]) = {
    val result = new js.Array[js.Any]

    for (element <- iterable) result.push(element)

    result
  }

  private def mapToObject(map: Map[String, js.Array[js.Any]]) = {
    val result = js.Dictionary.empty[js.Any]

    for ((key, values) <- map) result(key) = values

    result
  }

  private def exportNode(node: Node) = {
    val parentId = node.getParent.map(_.getId).getOrElse(-1)

    js.Dynamic.literal(
      "id" -> node.getId,
      "parent" -> parentId,
      "children" ->
        toJsArray(node.getBranches.map(_._2).flatten.map(_.getId: js.Any)),
      "kind" -> node.getKind,
      "values" -> mapToObject(node.getValues
        .map {
          case (key, values) =>
            key -> toJsArray(values.map(s => s: js.Any))
        }
      )
    )
  }

  private def tokenCursor(token: TokenReference, after: Boolean = false) = {
    val pair = token.collection.cursor(token.index + (if (after) 1 else 0))

    js.Dynamic.literal("line" -> (pair._1 - 1), "ch" -> (pair._2 - 1))
  }

}

case class StructureException(dynamic: Dynamic) extends RuntimeException
