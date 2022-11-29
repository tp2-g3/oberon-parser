package oberonParser

import cats.parse.Rfc5234.{sp, alpha, digit, char}
import cats.parse.{Parser, Parser0}
import cats.data.NonEmptyList
import oberonAST.*

object OberonParser {
	private val whitespaceP: Parser[Unit] = Parser.charIn(" \t\r\n").void
	private val whitespacesP: Parser0[Unit] = whitespaceP.rep0.void

	val identifierP: Parser[String] = (alpha ~ (alpha | digit).rep0).map((x, xs) => x :: xs)
		.map(s => s.mkString)

	val identifierDefP: Parser[String] = 
		(identifierP ~ (Parser.char('*').map(x => "*") | Parser.pure("")))
		.map((ident, x) => ident + x)

	// Implemented according to the ANTLR parser. 
	val qualifiedNameP = 
		(((identifierP ~ Parser.string("::")).map((a, _) => a + "::").backtrack | Parser.pure("")) ~
		identifierP).map((a, b) => a + b)

	private def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

	def realP: Parser[RealValue] = (digit.rep.map(nonEmptyListToInt) ~ (Parser.char('.') *>
		digit.rep0.map(x => "0." + x.mkString)).map(x => x.toDouble))
		.map((intPart, fracPart) => intPart.toDouble + fracPart)
		.map(RealValue.apply)

	def decIntegerP: Parser[IntValue] = 
		digit.rep
		.map(nonEmptyListToInt.apply)
		.map(IntValue.apply)

	def numberP: Parser[Number] = realP.backtrack | decIntegerP

	def quoteStringP: Parser[StringValue] = 
		Parser.charsWhile(x => x != '"')
		.surroundedBy(Parser.char('"'))
		.map(StringValue.apply)
	
	def charP: Parser[CharValue] = alpha.surroundedBy(Parser.char('\'')).map(CharValue.apply)

	def boolP: Parser[BoolValue] = Parser.string("True").map(x => BoolValue(true)) | 
		Parser.string("False").map(x => BoolValue(false))
	
	def nullP: Parser[Expression] = Parser.string("NIL").map(x => NullValue)
}