package name.lakhin.eliah.projects
package papacarlo.examples

import name.lakhin.eliah.projects.papacarlo.lexis.Matcher._
import name.lakhin.eliah.projects.papacarlo.{Lexer, Syntax}
import name.lakhin.eliah.projects.papacarlo.lexis.{Contextualizer, Token, _}
import name.lakhin.eliah.projects.papacarlo.syntax.Expressions._
import name.lakhin.eliah.projects.papacarlo.syntax.Rule._
import name.lakhin.eliah.projects.papacarlo.syntax.{Expressions, Rule}

/**
  * Created by den on 09.06.16.
  */

object DtLang {
  private def tokenizer = {
    val tokenizer = new Tokenizer()

    import Matcher._
    import tokenizer._

    tokenCategory("whitespace",
      oneOrMore(anyOf(" \t\f\n"))
    ).skip

    tokenCategory("string",
      sequence(
        chunk("\""),
        zeroOrMore(choice(
          anyExceptOf("\n\r\\\""),
          sequence(chunk("\\"), anyOf("\"\\/bfnrt")),
          sequence(
            chunk("\\u"),
            repeat(
              choice(rangeOf('a', 'f'), rangeOf('A', 'F'), rangeOf('0', '9')),
              times = 4
            )
          )
        )),
        chunk("\"")
      )
    )

    tokenCategory("number",
      sequence(
        optional(chunk("-")),
        choice(
          chunk("0"),
          sequence(rangeOf('1', '9'), zeroOrMore(choice(rangeOf('0', '9'), chunk("_"))))
        ),
        optional(sequence(chunk("."), oneOrMore(choice(rangeOf('0', '9'), chunk("_")))))
      )
    )

    tokenCategory("name",
      sequence(
        choice(rangeOf('a', 'z'), rangeOf('A', 'Z'), chunk("_")),
        zeroOrMore(choice(rangeOf('a', 'z'), rangeOf('A', 'Z'), rangeOf('0', '9'), chunk("_")))
      )
    )

    terminals(
      ";", ".", ",", ":=", ":",
      "[", "]", "(", ")",
      "+", "-", "*", "/", "%",
      "<=", "<", ">=", ">", "!=", "=",
      "|", "&", "!", "~",
      "//", "/*", "*/"
    )

    tokenizer
  }

  private def contextualizer = {
    val contextualizer = new Contextualizer

    import contextualizer._

    trackContext("[", "]").allowCaching
    trackContext("(", ")").allowCaching
    trackContext("//", Token.LineBreakKind).forceSkip.topContext
    trackContext("/*", "*/").forceSkip.topContext

    contextualizer
  }

  def lexer = new Lexer(tokenizer, contextualizer)

  def syntax(lexer: Lexer) = new {
    val syntax = new Syntax(lexer)

    import Expressions._
    import Rule._
    import syntax._

    val expr: Rule = rule("expr").cachable.main {
      val rule = expression(branch("operand", atom))

      var p = 1
      group(rule, "(", ")")
      prefix(rule, "+", p)
      prefix(rule, "-", p)
      prefix(rule, "!", p)

      p += 1
      infix(rule, "*", p)
      infix(rule, "/", p)
      infix(rule, "%", p)

      p += 1
      infix(rule, "+", p)
      infix(rule, "-", p)

      p += 1
      infix(rule, "~", p)

      p += 1
      infix(rule, "<", p)
      infix(rule, ">", p)
      infix(rule, "<=", p)
      infix(rule, ">=", p)

      p += 1
      infix(rule, "=", p)
      infix(rule, "!=", p)

      p += 1
      infix(rule, "&", p)

      p += 1
      infix(rule, "|", p)

      rule
    }

    val index = rule("index") {
      sequence(
        token("["),
        optional(sequence(
          branch("field", token("name")), // TODO capture token anywhere
          token(":")
        )),
        branch("filter", expr),
        recover(token("]"), "filter must end with ] sign")
      )
    }

    val segment = rule("segment") {
      sequence(
        capture("name", token("name")),
        branch("index", zeroOrMore(index))
      )
    }

    val path = rule("path") {
      oneOrMore(
        branch("segment", segment),
        separator =
          recover(token("."), "path entries must be separated with . sign")
      )
    }

    val fun = rule("fun") {
      sequence(
        token("name"),
        token("("),
        zeroOrMore(
          branch("expr", expr),
          separator =
            recover(token(","), "function arguments must be separated with , sign")
        ),
        recover(token(")"), "function call must end with ) sign")
      )
    }

    val atom = rule("atom") {
      choice(
        token("string"),
        token("number"),
        branch("fun", fun),
        branch("path", path)
      )
    }

  }.syntax

}
