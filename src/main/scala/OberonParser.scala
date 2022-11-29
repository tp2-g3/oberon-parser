package oberonParser

import cats.parse.Rfc5234.{sp, alpha, digit, char}
import cats.parse.{Parser, Parser0}
import cats.data.NonEmptyList
import oberonAST.*

object ParserSyntax {
	implicit class ParserOps[A](p: Parser[A]) {
		import OberonParser.*

		val whitespaceP: Parser[Unit] = Parser.charIn(" \r\t\n").void
		val whitespacesP: Parser[Unit] = whitespaceP.rep.void

		val commentP: Parser[Unit] = Parser.string("/*") *> 
			Parser.repUntil0(Parser.anyChar, Parser.string("*/").void).void

		val junkP: Parser0[Unit] = (whitespacesP | commentP).rep0.void

		def trim: Parser[A] = junkP.with1 *> p <* junkP

		def token: Parser[A] = p <* junkP

		def betweenBraces = p.between(charTokenP('('), charTokenP(')')) 
		def betweenBrackets = p.between(charTokenP('['), charTokenP(']')) 
	}
}

object OberonParser {
	import ParserSyntax.*

	private[oberonParser] def charTokenP(c: Char): Parser[Unit] = Parser.char(c).token
	
	val identifierP: Parser[String] = (alpha ~ (alpha | digit).rep0).map((x, xs) => x :: xs)
		.map(s => s.mkString)

	val identifierDefP: Parser[String] = 
		(identifierP ~ (Parser.char('*').map(x => "*") | Parser.pure("")))
		.map((ident, x) => ident + x)

	val qualifiedNameP: Parser[String] = 
		(((identifierP ~ Parser.string("::")).map((a, _) => a + "::").backtrack | Parser.pure("")).with1 ~
		identifierP).map((a, b) => a + b)

	private def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

	def realP: Parser[RealValue] = (digit.rep.map(nonEmptyListToInt) ~ (Parser.char('.').token *>
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

	def expValueP: Parser[Expression] = 
		decIntegerP | realP | charP | quoteStringP | boolP | nullP

	def argumentsP: Parser[List[Expression]] = expressionP.repSep(Parser.char(',').token)
		.map(x => x.toList)

	def functionCallP: Parser[FunctionCallExpression] = 
		(qualifiedNameP ~ argumentsP.betweenBraces)
		.map(FunctionCallExpression.apply)
	
	def varExpressionP: Parser[VarExpression] = qualifiedNameP.map(VarExpression.apply)

	def fieldAccessP: Parser[FieldAccessExpression] = 
		((expressionP <* charTokenP('.')) ~ identifierP)
		.map(FieldAccessExpression.apply)

	def arraySubscriptP: Parser[ArraySubscript] = 
		(expressionP ~ expressionP.betweenBrackets)
		.map(ArraySubscript.apply)

	def expressionP: Parser[Expression] =
		expressionP.token.betweenBraces.token |
		expValueP.token |
		functionCallP.backtrack.token |
		varExpressionP.token |
		fieldAccessP.token |
		arraySubscriptP.token
}