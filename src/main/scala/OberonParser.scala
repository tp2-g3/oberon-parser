package oberonParser

import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.{Parser, Parser0}

sealed trait AddOperator
case object PlusOperator extends AddOperator
case object MinusOperator extends AddOperator
case object OrOperator extends AddOperator

sealed trait MulOperator
case object TimesOperator extends MulOperator
case object SlashOperator extends MulOperator
case object DivOperator extends MulOperator
case object ModOperator extends MulOperator
case object AndOperator extends MulOperator

final case class Identifier(name: String)

val whitespaceP: Parser[Unit] = Parser.charIn(" \t\r\n").void
val whitespacesP: Parser0[Unit] = whitespaceP.rep0.void

val addOperatorP: Parser[AddOperator] = 
Parser.char('+').map(x => PlusOperator) | Parser.char('-').map(x => MinusOperator) |
Parser.string("OR").map(x => OrOperator)

val mulOperatorP: Parser[MulOperator] = 
Parser.char('*').map(x => TimesOperator) | Parser.char('/').map(x => SlashOperator) |
Parser.string("DIV").map(x => DivOperator) | Parser.string("MOD").map(x => ModOperator) |
Parser.char('&').map(x => AndOperator)

val identifierP: Parser[Identifier] = (alpha ~ (alpha | digit).rep0).map((x, xs) => x :: xs).
map(s => Identifier(s.toString))

