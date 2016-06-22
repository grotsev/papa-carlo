package kz.greetgo.dtlang

import name.lakhin.eliah.projects.papacarlo.test.utils.ParserSpec

/**
  * Created by den on 10.06.16.
  */
class DtLangSpec extends ParserSpec("dtlang") {
  override def lexer = DtLang.lexer
  override def parser = {
    val lexer = DtLang.lexer
    val syntax = DtLang.syntax(lexer)

    (lexer, syntax)
  }

  def main(args: Array[String]) {
    println("Hello")
  }
}
