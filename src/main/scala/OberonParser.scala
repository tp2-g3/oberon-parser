package oberonParser

import cats.parse.Rfc5234.{alpha, digit, char}
import cats.parse.{Parser, Parser0}
import cats.data.NonEmptyList
import oberonAST.*

object ParserSyntax {
	implicit class ParserOps[A](p: Parser[A]) {
		import OberonParser.*

		val whitespaceP: Parser[Unit] = Parser.charIn(" \r\t\n").void
		val whitespacesP: Parser[Unit] = whitespaceP.rep.void

		val commentP: Parser[Unit] = 
			Parser.string("/*") *> 
			Parser.anyChar.repUntil0(Parser.string("*/")).void <*
			Parser.string("*/")

		val junkP: Parser0[Unit] = (whitespacesP | commentP).rep0.void

		val trim: Parser[A] = junkP.with1 *> p <* junkP

		val token: Parser[A] = p <* junkP

		def parseString(str: String) = p.trim.parse(str)

		def betweenParen: Parser[A] = p.between(charTokenP('('), charTokenP(')')) 
		def betweenBrackets: Parser[A] = p.between(charTokenP('['), charTokenP(']')) 
	}
}

object OberonParser {
	import ParserSyntax.*

	private[oberonParser] def charTokenP(c: Char): Parser[Unit] = Parser.char(c).token
	private def stringTokenP(str: String): Parser[Unit] = Parser.string(str).token


	val identifierP: Parser[String] = 
		(alpha ~ (alpha | digit).rep0)
		.map((x, xs) => x :: xs)
		.map(s => s.mkString).token

	val identifierDefP: Parser[String] = 
		(identifierP ~ (Parser.char('*').map(x => "*") | Parser.pure("")))
		.map((ident, x) => ident + x).token

	val qualifiedNameHelperP: Parser[String] =
		(identifierP <* Parser.string("::").token).map(a => a + "::").backtrack

	val qualifiedNameP: Parser[String] = 
		((qualifiedNameHelperP.backtrack | Parser.pure("")).with1 ~ identifierP)
		.map((a, b) => a + b).token

	private def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

	def unsignedRealP: Parser[RealValue] = 
		(digit.rep.map(nonEmptyListToInt) ~ (charTokenP('.') *>
		digit.rep0.map(x => "0." + x.mkString)).map(x => x.toDouble))
		.map((intPart, fracPart) => intPart.toDouble + fracPart)
		.map(RealValue.apply)

	private def signP: Parser[UnaryArithOperator] =
		charTokenP('+').map(x => UnaryPlusOperator) | charTokenP('-').map(x => UnaryMinusOperator)

	def realP: Parser[RealValue] =
		(signP.?.with1 ~ unsignedRealP)
		.map { case (sign, RealValue(num)) => 
			sign match {
				case None => RealValue(num)
				case Some(UnaryPlusOperator) => RealValue(num)
				case Some(UnaryMinusOperator) => RealValue(-num)
			}
		}.token

	def unsignedDecIntegerP: Parser[IntValue] =
		digit.rep
		.map(nonEmptyListToInt.apply)
		.map(IntValue.apply)

	def decIntegerP: Parser[IntValue] =
		(signP.?.with1 ~ unsignedDecIntegerP)
		.map { case (sign, IntValue(num)) => 
			sign match {
				case None => IntValue(num)
				case Some(UnaryPlusOperator) => IntValue(num)
				case Some(UnaryMinusOperator) => IntValue(-num)
			}
		}.token

	def numberP: Parser[Number] = realP.backtrack | decIntegerP

	def quoteStringP: Parser[StringValue] =
		Parser.charsWhile(x => x != '"')
		.surroundedBy(Parser.char('"'))
		.map(StringValue.apply).token

	def charP: Parser[CharValue] = 
		alpha.surroundedBy(Parser.char('\''))
		.map(CharValue.apply).token

	def boolP: Parser[BoolValue] =
		stringTokenP("True").map(x => BoolValue(true)).token |
		stringTokenP("False").map(x => BoolValue(false)).token

	def nullP: Parser[Expression] = 
		stringTokenP("NIL")
		.map(x => NullValue).token

	def expValueP: Parser[Expression] = 
		decIntegerP | realP | charP | quoteStringP | boolP | nullP

	def varExpressionP: Parser[VarExpression] = qualifiedNameP.map(VarExpression.apply)

	def fieldAccessExprP(exprRecP: Parser[Expression]): Parser[Expression] =
		((exprRecP.betweenParen | functionCallP(exprRecP).backtrack | varExpressionP) ~ (charTokenP('.') *> identifierP).rep)
		.map { case (expr, names: NonEmptyList[String]) =>
			names.toList.foldLeft(expr:Expression)((acc, name) => FieldAccessExpression(acc, name))
		}

	def arraySubscriptP(exprRecP: Parser[Expression]): Parser[ArraySubscript] =
		(exprRecP ~ exprRecP.betweenBrackets)
		.map(ArraySubscript.apply)

	def pointerAccessP: Parser[PointerAccessExpression] =
		(qualifiedNameP <* charTokenP('^'))
		.map(PointerAccessExpression.apply)

	def relationP: Parser[RelationOperator] =
		stringTokenP("=").map(x => EQOperator) |
		stringTokenP("#").map(x => NEQOperator) |
		stringTokenP("<=").map(x => LTEOperator).backtrack |
		stringTokenP("<").map(x => LTOperator) |
		stringTokenP(">=").map(x => GTEOperator).backtrack |
		stringTokenP(">").map(x => GTOperator)

	def addP: Parser[AddOperator] =
		stringTokenP("+").map(x => PlusOperator) |
		stringTokenP("-").map(x => MinusOperator) | stringTokenP("||").map(x => OrOperator)

	def multP: Parser[MultOperator] =
		stringTokenP("*").map(x => TimesOperator) | stringTokenP("&&").map(x => AndOperator) |
		stringTokenP("/").map(x => SlashOperator) | stringTokenP("MOD").map(x => ModOperator)

	def notFactorP(facRecP: Parser[Expression]): Parser[Expression] =
		(charTokenP('~') *> facRecP)
		.map(NotExpression.apply)

	def exprListP(exprRecP: Parser[Expression]): Parser[List[Expression]] =
		exprRecP.repSep(charTokenP(','))
		.map(x => x.toList)

	def actualParametersP(exprRecP: Parser[Expression]): Parser[List[Expression]] =
		exprListP(exprRecP).betweenParen

	def exprDesignatorP(exprRecP: Parser[Expression]): Parser[Expression] =
		(qualifiedNameP ~ selectorP(exprRecP).?)
		.map { case (name, optionSelector: Option[Selector]) =>
			optionSelector match {
				case None => VarExpression(name)
				case Some(PointerSelector) => PointerAccessExpression(name) 
				case Some(ArraySelector(expr)) => ArraySubscript(VarExpression(name), expr)
			}
		}

	def functionCallP(exprRecP: Parser[Expression]): Parser[FunctionCallExpression] =
		(qualifiedNameP ~ actualParametersP(exprRecP))
		.map(FunctionCallExpression.apply)

	def selectorP(exprRecP: Parser[Expression]): Parser[Selector] =
		charTokenP('^').map(x => PointerSelector) |
		exprRecP.betweenBrackets.map(ArraySelector.apply)


	def factorP(exprRecP: Parser[Expression]): Parser[Expression] = Parser.recursive { facRecP =>
		numberP | quoteStringP | nullP.backtrack | boolP.backtrack |
		fieldAccessExprP(exprRecP).backtrack |
		functionCallP(exprRecP).backtrack | exprDesignatorP(exprRecP).backtrack |
		exprRecP.betweenParen | notFactorP(facRecP)
	}

	def termP(exprRecP: Parser[Expression]): Parser[Expression] = {
		(factorP(exprRecP) ~ (multP ~ factorP(exprRecP)).rep0)
		.map { case (x: Expression, xs: List[(MultOperator, Expression)]) =>
			xs.foldLeft(x){ case (expr, (opr, acc)) =>
				opr match {
					case TimesOperator => MultExpression(expr, acc)
					case AndOperator => AndExpression(expr, acc)
					case SlashOperator => DivExpression(expr, acc)
					case ModOperator => ModExpression(expr, acc)
				}
			}
		}
	}

	def simpleExpressionP(exprRecP: Parser[Expression]): Parser[Expression] =
		(termP(exprRecP) ~ (addP ~ termP(exprRecP)).rep0)
		.map { case (x: Expression, xs: List[(AddOperator, Expression)]) =>
			xs.foldLeft(x){ case (expr, (opr, acc)) =>
				opr match {
					case PlusOperator => AddExpression(expr, acc)
					case MinusOperator => SubExpression(expr, acc)
					case OrOperator => OrExpression(expr, acc)
				}
			}
		}

	def expressionP: Parser[Expression] = Parser.recursive { exprRecP =>
		(simpleExpressionP(exprRecP) ~ (relationP ~ simpleExpressionP(exprRecP)).?)
		.map { (expr1: Expression, optionExpr2: Option[(RelationOperator, Expression)]) =>
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
}