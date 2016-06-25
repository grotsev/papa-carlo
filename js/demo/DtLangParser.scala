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
import js.annotation.{JSName, JSExport}

import name.lakhin.eliah.projects.papacarlo.lexis.TokenReference
import name.lakhin.eliah.projects.papacarlo.syntax.Node
import kz.greetgo.dtlang.DtLang

@JSExport("DtLangParser")
object DtLangParser {
  private val lexer = DtLang.lexer
  private val syntax = DtLang.syntax(lexer)
  private var addedNodes = List.empty[Int]
  private var removedNodes = List.empty[Int]

  syntax.onNodeCreate.bind { node => addedNodes ::= node.getId }
  syntax.onNodeRemove.bind { node => removedNodes ::= node.getId }

  @JSExport
  def allNodeList(text: String) = {
    lexer.input(text)
    val array = new js.Array[js.Any]
    if (syntax.getRootNode.isDefined)
      extractStatements(array, syntax.getRootNode.get);
    array
  }

  val statements = List("seq", "switch", "case", "call", "let", "for", "break", "continue")

  def extractStatements(array: js.Array[js.Any], node: Node, parent: Option[Int] = None): Unit = {
    for (
      result <- node.getBranches.getOrElse("result", List.empty);
      call <- result.getBranches.getOrElse("call", List.empty);
      path <- result.getBranches.getOrElse("path", List.empty);
      segment <- path.getBranches.getOrElse("segment", List.empty);
      name <- segment.getValues.getOrElse("name", List.empty);
      if (statements contains name)
    ) {
      val id = node.getId
      val element = js.Dynamic.literal(
        "id" -> id,
        "parent" -> parent.orUndefined,
        "text" -> name,
        "type" -> name
      )
      array.push(element)
      for (subExpr <- call.getBranches.getOrElse("expr", List.empty))
        extractStatements(array, subExpr, Some(id))
    }
  }

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
    syntax.getNode(id) match {
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

    //result("total") = syntax.nodes.size
    result("added") = toJsArray(addedNodes.reverse.map(x => x: js.Any))
    result("removed") = toJsArray(removedNodes.reverse.map(x => x: js.Any))

    if (graph) {
      val ast = js.Dictionary.empty[js.Any]

      /* TODO
            for (node <- syntax.nodes.elements) {
              ast(node.getId.toString) = exportNode(node)
            }
      */

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

