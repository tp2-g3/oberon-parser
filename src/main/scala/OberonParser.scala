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

		val trim: Parser[A] = junkP.with1 *> p <* junkP

		val token: Parser[A] = p <* junkP

		def betweenBraces: Parser[A] = p.between(charTokenP('('), charTokenP(')')) 
		def betweenBrackets: Parser[A] = p.between(charTokenP('['), charTokenP(']')) 
	}
}

object OberonParser {
	import ParserSyntax.*

	private[oberonParser] def charTokenP(c: Char): Parser[Unit] = Parser.char(c).token
	
	val identifierP: Parser[String] = 
		(alpha ~ (alpha | digit).rep0)
		.map((x, xs) => x :: xs)
		.map(s => s.mkString)

	val identifierDefP: Parser[String] = 
		(identifierP ~ (Parser.char('*').map(x => "*") | Parser.pure("")))
		.map((ident, x) => ident + x)

	val qualifiedNameHelperP: Parser[String] = 
		(identifierP <* Parser.string("::")).map(a => a + "::").backtrack

	val qualifiedNameP: Parser[String] = 
		((qualifiedNameHelperP.backtrack | Parser.pure("")).with1 ~ identifierP)
		.map((a, b) => a + b)

	private def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

	def realP: Parser[RealValue] = 
		(digit.rep.map(nonEmptyListToInt) ~ (Parser.char('.').token *>
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

	def boolP: Parser[BoolValue] = 
		Parser.string("True").map(x => BoolValue(true)) | 
		Parser.string("False").map(x => BoolValue(false))
	
	def nullP: Parser[Expression] = Parser.string("NIL").map(x => NullValue)

	def expValueP: Parser[Expression] = 
		decIntegerP | realP | charP | quoteStringP | boolP | nullP

	def argumentsP: Parser[List[Expression]] = 
		expressionP.repSep(Parser.char(',').token)
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

	def pointerAccessP: Parser[PointerAccessExpression] =
		(identifierP <* charTokenP('^'))
		.map(PointerAccessExpression.apply)
	
	def notExprP: Parser[NotExpression] = 
		(charTokenP('~') *> expressionP)
		.map(NotExpression.apply)

	def oprExprP(opr: String): Parser[(Expression, Expression)] = 
		(expressionP <* Parser.string(opr).token.void) ~ expressionP

	def eqExprP: Parser[EQExpression] = oprExprP("=").map(EQExpression.apply)
	def neqExprP: Parser[NEQExpression] = oprExprP("#").map(NEQExpression.apply)
	def ltExprP: Parser[LTExpression] = oprExprP("<").map(LTExpression.apply)
	def lteExprP: Parser[LTEExpression] = oprExprP("<=").map(LTEExpression.apply)
	def gtExprP: Parser[GTExpression] = oprExprP(">").map(GTExpression.apply)
	def gteExprP: Parser[GTEExpression] = oprExprP(">=").map(GTEExpression.apply)
	def timesExprP: Parser[MultExpression] = oprExprP("*").map(MultExpression.apply) 
	def divExprP: Parser[MultExpression] = oprExprP("/").map(MultExpression.apply) 
	def andExprP: Parser[MultExpression] = oprExprP("&&").map(MultExpression.apply) 
	def plusExprP: Parser[AddExpression] = oprExprP("+").map(AddExpression.apply) 
	def modExprP: Parser[AddExpression] = oprExprP("MOD").map(AddExpression.apply) 
	def minusExprP: Parser[AddExpression] = oprExprP("-").map(AddExpression.apply) 
	def orExprP: Parser[AddExpression] = oprExprP("||").map(AddExpression.apply) 

	def relExprP: Parser[Expression] =
		eqExprP | neqExprP | ltExprP | lteExprP | gteExprP | gteExprP

	def multExpressionP: Parser[MultExpression] =
		timesExprP | divExprP | andExprP
	
	def addExpressionP: Parser[AddExpression] = 
		plusExprP | modExprP | minusExprP | orExprP

	def expressionP: Parser[Expression] =
		expressionP.token.betweenBraces.token |
		expValueP.token |
		functionCallP.backtrack.token |
		varExpressionP.token |
		fieldAccessP.token |
		arraySubscriptP.token |
		pointerAccessP.token |
		relExprP.backtrack.token |
		multExpressionP.backtrack.token |
		addExpressionP.backtrack.token
}