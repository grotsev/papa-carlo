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

//import name.lakhin.eliah.projects.papacarlo.examples.Json

import scala.scalajs.js.UndefOr

/**
  * Created by den on 20.06.16.
  */
@JSExport("DtLangParser")
object DtLangParser {
  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)
  private var lastAddedNode = Option.empty[Int]
  private var lastRemovedNode = Option.empty[Int]

  private var addedNodes = List.empty[Int]
  // TODO remove
  private var removedNodes = List.empty[Int]
  syntax.onNodeCreate.bind { node => addedNodes ::= node.getId }
  syntax.onNodeRemove.bind { node => removedNodes ::= node.getId }


  private var firstAddedExpr = Option.empty[Int]

  syntax.onNodeCreate.bind { node => // Root is last
    if (firstAddedExpr.isEmpty && node.getKind == "expr")
      firstAddedExpr = Some(node.getId)
  }

  //syntax.onNodeRemove.bind { node => lastRemovedNode = Some(node.getId) }
  // Is root last or first?

  val statements = List(
    "empty", "assign", "group",
    "condition", "case",
    "foreach", "break", "continue",
    "procedure", "exit", "stop", "message", "error"
  )

  @JSExport
  def replace(text: String, oldId: UndefOr[Int]): UndefOr[js.Dynamic] = {
    firstAddedExpr = None
    val node: Node = if (oldId.isDefined) {
      val oldNode: Node = syntax.getNode(oldId.get).get // WARN should find, otherwise bug is somewhere
      lexer.input(text, tokenPos(oldNode.getBegin), tokenPos(oldNode.getEnd, after = true))
      val n = firstAddedExpr;
      syntax.getNode(n.get).get // should be Some
    } else {
      lexer.input(text)
      syntax.getRootNode.get // should be Some
    }
    firstAddedExpr = None
    extractStats(node).orUndefined;
  }

  def extractStats(node: Node, parent: Option[Int] = None): Option[js.Dynamic] = {
    for (
      result <- node.getBranches.getOrElse("result", List.empty);
      call <- result.getBranches.getOrElse("call", List.empty);
      path <- result.getBranches.getOrElse("path", List.empty);
      segment <- path.getBranches.getOrElse("segment", List.empty);
      name <- segment.getValues.getOrElse("name", List.empty);
      if (statements contains name)
    ) {
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
            if (exprs.isEmpty) "break" else "break " + exprs.get(0).sourceCode
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
          case _ => name
        }),
        "type" -> name
      )
      val children = new js.Array[js.Any]
      for (subExpr <- call.getBranches.getOrElse("expr", List.empty)) {
        val child = extractStats(subExpr, Some(id))
        child.foreach(children.push(_))
      }
      if (children.length > 0) element.updateDynamic("children")(children)
      return Some(element)
    }
    None
  }

  private def tokenPos(token: TokenReference, after: Boolean = false) = {
    val c = token.collection.cursor(token.index + (if (after) 1 else 0))
    (c._1 - 1, c._2 - 1)
  }

  @JSExport
  def register(add: js.Dynamic, remove: js.Dynamic) = {
    syntax.onNodeCreate.bind {
      node => {

        node.getKind match {
          case "call" => {
            node.onAddBranch.bind { expr => {
              val parent = node.getParent.get.getParent.get.getId
              val child = js.Dynamic.literal(
                "id" -> expr.getId,
                //"parent" = parent // possible unused
                "text" -> ("ok" + expr.getId.toString), // TODO
                "type" -> "type" // TODO
              )
              val offset = node.getBranches("expr").indexOf(expr)
              add(parent, child, offset)
            }
            } // TODO
          }
          case "expr" => {
            if (node.getId == 1) {
              val child = js.Dynamic.literal(
                "id" -> node.getId,
                //"parent" = parent // possible unused
                "text" -> ("ok" + node.getId.toString), // TODO
                "type" -> "type" // TODO
              )
              add("#", child)
            }
            node.onRemove.bind { expr => remove(expr.getId) }
          }
          case _ =>
        }

      }
    }
  }

  @JSExport
  def onExprMerge(callback: js.Dynamic) = {
    // TODO remove
    syntax.onNodeMerge.bind {
      node => {
        var expr = node;
        while (expr.getKind != "expr")
          expr = expr.getParent.get;
        callback(expr.getId)
      }
    }
  }

  @JSExport
  def getExpr(id: Int): UndefOr[js.Dynamic] = {
    for (
      node <- syntax.getNode(id);
      result <- node.getBranches.getOrElse("result", List.empty);
      call <- result.getBranches.getOrElse("call", List.empty);
      path <- result.getBranches.getOrElse("path", List.empty);
      segment <- path.getBranches.getOrElse("segment", List.empty);
      name <- segment.getValues.getOrElse("name", List.empty);
      if (statements contains name)
    ) {
      val id = node.getId
      val e = (i: Int) => call.getBranches("expr")(i).sourceCode
      val element = js.Dynamic.literal(
        "id" -> id,
        "parent" -> (for (call <- node.getParent; result <- call.getParent; expr <- result.getParent) yield expr.getId.toString).getOrElse[String]("#"),
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
            if (exprs.isEmpty) "break" else "break " + exprs.get(0).sourceCode
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
          case _ => name
        }),
        "type" -> name
      )
      val children = new js.Array[js.Any]
      // println(call.getBranches)
      for (subExpr <- call.getBranches.getOrElse("expr", List.empty); if subExpr.getKind == "expr") {
        children.push(subExpr.getId)
      }
      if (children.length > 0) element.updateDynamic("children")(children)
      return element
    }
    js.undefined
  }

  // for demo

  @JSExport
  def inputAll(text: String) {
    lexer.input(text)
  }

  @JSExport
  def input(text: String,
            fromLine: Int,
            fromChar: Int,
            toLine: Int,
            toChar: Int) {
    lexer.input(text, fromLine -> fromChar, toLine -> toChar)
  }

  @JSExport
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

