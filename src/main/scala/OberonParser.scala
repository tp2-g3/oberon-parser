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

	def argumentsP(recP: Parser[Expression]): Parser[List[Expression]] =
		recP.repSep(Parser.char(',').token)
		.map(x => x.toList)

	def functionCallP(recP: Parser[Expression]): Parser[FunctionCallExpression] =
		(qualifiedNameP ~ argumentsP(recP).betweenBraces)
		.map(FunctionCallExpression.apply)
	
	def varExpressionP: Parser[VarExpression] = qualifiedNameP.map(VarExpression.apply)

	def fieldAccessP(recP: Parser[Expression]): Parser[FieldAccessExpression] =
		((recP <* charTokenP('.')) ~ identifierP)
		.map(FieldAccessExpression.apply)

	def arraySubscriptP(recP: Parser[Expression]): Parser[ArraySubscript] =
		(recP ~ recP.betweenBrackets)
		.map(ArraySubscript.apply)

	def pointerAccessP: Parser[PointerAccessExpression] =
		(identifierP <* charTokenP('^'))
		.map(PointerAccessExpression.apply)
	
	def notExprP(recP: Parser[Expression]): Parser[NotExpression] = 
		(charTokenP('~') *> recP)
		.map(NotExpression.apply)

	def oprExprP(opr: String, recP: Parser[Expression]): Parser[(Expression, Expression)] = 
		(recP <* Parser.string(opr).token.void) ~ recP

	def eqExprP(recP: Parser[Expression]) = oprExprP("=", recP).map(EQExpression.apply)
	def neqExprP(recP: Parser[Expression]) = oprExprP("#", recP).map(NEQExpression.apply)
	def ltExprP(recP: Parser[Expression]) = oprExprP("<", recP).map(LTExpression.apply)
	def lteExprP(recP: Parser[Expression]) = oprExprP("<=", recP).map(LTEExpression.apply)
	def gtExprP(recP: Parser[Expression]) = oprExprP(">", recP).map(GTExpression.apply)
	def gteExprP(recP: Parser[Expression]) = oprExprP(">=", recP).map(GTEExpression.apply)
	def timesExprP(recP: Parser[Expression]) = oprExprP("*", recP).map(MultExpression.apply)
	def divExprP(recP: Parser[Expression]) = oprExprP("/", recP).map(MultExpression.apply)
	def andExprP(recP: Parser[Expression]) = oprExprP("&&", recP).map(MultExpression.apply)
	def plusExprP(recP: Parser[Expression]) = oprExprP("+", recP).map(AddExpression.apply)
	def modExprP(recP: Parser[Expression]) = oprExprP("MOD", recP).map(AddExpression.apply)
	def minusExprP(recP: Parser[Expression]) = oprExprP("-", recP).map(AddExpression.apply)
	def orExprP(recP: Parser[Expression]) = oprExprP("||", recP).map(AddExpression.apply)

	def relExprP(recP: Parser[Expression]): Parser[Expression] =
		eqExprP(recP) | neqExprP(recP) | ltExprP(recP) |
		lteExprP(recP) | gteExprP(recP) | gteExprP(recP)

	def multExpressionP(recP: Parser[Expression]): Parser[MultExpression] =
		timesExprP(recP) | divExprP(recP) | andExprP(recP)
	
	def addExpressionP(recP: Parser[Expression]): Parser[AddExpression] =
		plusExprP(recP) | modExprP(recP) | minusExprP(recP) | orExprP(recP)

	def relationP: Parser[RelationOperator] =
		Parser.string("=").map(x => EQOperator) | Parser.string("#").map(x => NEQOperator) |
		Parser.string("<").map(x => LTEOperator) | Parser.string("<=").map(x => LTEOperator) |
		Parser.string(">").map(x => GTOperator) | Parser.string(">=").map(x => GTEOperator)

	def simpleExpressionP: Parser[Expression] = ???

	def expressionP: Parser[Expression] = 
	(simpleExpressionP ~ (relationP.token ~ simpleExpressionP).?)
	.map { (expr1, optionExpr2) =>
		optionExpr2 match {
			case None => expr1
			case Some((EQOperator, expr2)) => EQExpression(expr1, expr2)
			case Some((NEQOperator, expr2)) => NEQExpression(expr1, expr2)
			case Some((LTOperator, expr2)) => LTExpression(expr1, expr2)
			case Some((LTEOperator, expr2)) => LTEExpression(expr1, expr2)
			case Some((GTOperator, expr2)) => GTExpression(expr1, expr2)
			case Some((GTEOperator, expr2)) => GTEExpression(expr1, expr2)
		}
	}
}