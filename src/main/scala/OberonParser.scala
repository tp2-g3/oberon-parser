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

		def parseString(str: String) = p.trim.parse(str)

		def betweenParen: Parser[A] = p.between(charTokenP('('), charTokenP(')')) 
		def betweenBrackets: Parser[A] = p.between(charTokenP('['), charTokenP(']')) 
	}
}

object OberonParser {
	import ParserSyntax.*

	def charTokenP(c: Char): Parser[Unit] = Parser.char(c).token
	private def stringTokenP(str: String): Parser[Unit] = Parser.string(str).token

	val identifierP: Parser[String] = 
		(alpha ~ (alpha | digit).rep0)
		.map((x, xs) => x :: xs)
		.map(s => s.mkString).token

	val identifierDefP: Parser[String] = 
		(identifierP ~ (Parser.char('*').map(x => "*") | Parser.pure("")))
		.map((ident, x) => ident + x).token

	val qualifiedNameHelperP: Parser[String] =
		(identifierP <* Parser.string("::")).map(a => a + "::").backtrack

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

	def selectorP(exprRecP: Parser[Expression]): Parser[Selector] =
		(charTokenP('.') *> identifierP).map(FieldSelector.apply).backtrack |
		exprRecP.betweenBrackets.map(ArraySelector.apply) |
		charTokenP('^').map(_ => PointerSelector)

	def designatorP(exprRecP: Parser[Expression]): Parser[DesignatorHelper] = 
		(qualifiedNameP ~ selectorP(exprRecP).rep0)
		.map(DesignatorHelper.apply)

	def designatorHelperToExpression(designator: DesignatorHelper): Expression =
		designator match {
			case DesignatorHelper(name, selectors) => {
				selectors.foldLeft(VarExpression(name): Expression){ (acc, value) =>
					value match {
						case PointerSelector => ComplexPointerExpression(acc)
						case ArraySelector(index) => ArraySubscript(acc, index)
						case FieldSelector(propName) => FieldAccessExpression(acc, propName)
					}
				}
			}
		}

	def designatorHelperToDesignator(designator: DesignatorHelper): Designator =
		designatorHelperToExpression(designator) match {
			case VarExpression(propName) => VarAssignment(propName)
			case FieldAccessExpression(acc,propName) => RecordAssignment(acc,propName)
			case ArraySubscript(acc,index) => ArrayAssignment(acc,index)
			case ComplexPointerExpression(VarExpression(name)) => PointerAssignment(name)
			case ComplexPointerExpression(acc) => ComplexPointerAssignment(acc)
		}

	def exprDesignatorP(exprRecP: Parser[Expression]): Parser[Expression] =
		(designatorP(exprRecP) ~ actualParametersP(exprRecP).?)
		.map{ case (designator, optionParams) => 
			optionParams match {
				case Some(params) => designator match {
					case DesignatorHelper(name, Nil) => 
					FunctionCallExpression(name, params)
					case designator =>
						ComplexFunctionCallExpression(designatorHelperToExpression(designator), params)
				}
				case None => designatorHelperToExpression(designator)
			}
		
		}
		.map( expr => 
			expr match {
				case ComplexPointerExpression(VarExpression(name)) => PointerAccessExpression(name)
				case _ => expr
			}
		)


	def factorP(exprRecP: Parser[Expression]): Parser[Expression] = Parser.recursive { facRecP =>
		numberP | quoteStringP | nullP.backtrack | boolP.backtrack |
		exprDesignatorP(exprRecP).backtrack |
		exprRecP.betweenParen | notFactorP(facRecP)
	}

	def termP(exprRecP: Parser[Expression]): Parser[Expression] = 
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

	def assignmentStmtP: Parser[Statement] = {
		((designatorP(expressionP) <* Parser.string(":=").token) ~ expressionP)
		.map((a, b) =>  AssignmentStmt(designatorHelperToDesignator(a),b))
	}
	
	def writeStmtP: Parser[Statement] = {
		(Parser.string("write") *> expressionP)
		.map(x => WriteStmt(x))
	}

	def procedureCallStmtP: Parser[Statement] = {
		(qualifiedNameP ~ (exprListP(expressionP).?).between(charTokenP('('), charTokenP(')')))
		.map{ (x,y) => (x,y) match { 
				case (x,None) => ProcedureCallStmt(x,Nil)
				case (x,Some(listinha)) => ProcedureCallStmt(x,listinha)
			}
		}
	}
	
	def readCharStmtP: Parser[Statement] = {
		(Parser.string("readChar") *> identifierP.betweenParen)
		.map(x => ReadCharStmt(x))
	}

	def readRealStmtP: Parser[Statement] = {
		(Parser.string("readReal") *> identifierP.betweenParen)
		.map(x => ReadRealStmt(x))
	}

	def readLongRealStmtP: Parser[Statement] = {
		(Parser.string("readLongReal") *> identifierP.betweenParen)
		.map(x => ReadLongRealStmt(x))
	}

	def readIntStmtP: Parser[Statement] = {
		(Parser.string("readInt") *> identifierP.betweenParen)
		.map(x => ReadIntStmt(x))
	}

	def readLongIntStmtP: Parser[Statement] = {
		(Parser.string("readLongInt") *> identifierP.betweenParen)
		.map(x => ReadLongIntStmt(x))
	}

	def readShortIntStmtP: Parser[Statement] = {
		(Parser.string("readShortInt") *> identifierP.betweenParen)
		.map(x => ReadShortIntStmt(x))
	}

	def ifStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		((Parser.string("IF").token *> expressionP <* Parser.string("THEN").token) ~ sequenceStmtP(stmtRecP) ~ 
		((Parser.string("ELSEIF").token *> expressionP <* Parser.string("THEN").token) ~ sequenceStmtP(stmtRecP)).rep0 ~ 
		(Parser.string("ELSE").token *> sequenceStmtP(stmtRecP)).? <* Parser.string("END").token)
		.map{ 
			case (((expr,statement1),(head1,head2)::listElseIf),statement2) => {
				val foldedList = listElseIf.foldLeft(List(ElseIfStmt(head1,head2))){ 
					case (x,(acc1,acc2)) => (x :+ ElseIfStmt(acc1,acc2)) 
				}
				IfElseIfStmt(expr,statement1,foldedList,statement2)
			}
			case((((expr,statement1),Nil),statement2)) => IfElseStmt(expr,statement1,statement2)
		}
	}

	def sequenceStmtP(stmtRecP: Parser0[Option[Statement]]): Parser0[Statement] = {
		(stmtRecP ~ (Parser.string(";").token *> stmtRecP).rep0)
		.map {
			case(Some(stmt),listStmt) => {
				val foldedList = listStmt.foldLeft(List(stmt)){
					case(x,Some(y)) => x :+ y 
					case(x,None) => x
				}
				SequenceStmt(foldedList)
			}
			case(None,_) => SequenceStmt(Nil)
		}
	}

	def whileStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		((Parser.string("WHILE").token *> expressionP <* Parser.string("DO").token) ~ 
		sequenceStmtP(stmtRecP) <* Parser.string("END").token)
		.map((x,y) => WhileStmt(x,y))
	}

	def repeatStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		((Parser.string("REPEAT").token *> sequenceStmtP(stmtRecP) <* Parser.string("UNTIL").token) ~ expressionP)
		.map((x,y) => RepeatUntilStmt(y,x))
	}

	def forStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		((Parser.string("FOR").token *> sequenceStmtP(stmtRecP) <* Parser.string("TO").token) ~
		expressionP ~(Parser.string("DO").token *> sequenceStmtP(stmtRecP) <* Parser.string("END").token))
		.map{case ((x,y),z) => ForStmt(x,y,z)}
	}

	def forEachStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		((Parser.string("FOREACH").token *> identifierP <* Parser.string("IN").token) ~ expressionP ~ 
		sequenceStmtP(stmtRecP) <* Parser.string("END").token)
		.map{case ((x,y),z) => ForEachStmt(x,y,z)}
	}
	
	def loopStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		(Parser.string("LOOP").token *> sequenceStmtP(stmtRecP) <* Parser.string("END").token)
		.map(statement => LoopStmt(statement))
	} 

	def returnP: Parser[Statement] = {
		(Parser.string("RETURN").token *> expressionP)
		.map((expression) => ReturnStmt(expression))
	}

	def exitP: Parser[Statement] = {
		(Parser.string("EXIT").token)
		.map(_ => ExitStmt())
	}

	def caseAlternativeP(stmtRecP: Parser0[Option[Statement]]): Parser[CaseAlternative] = {
		(expressionP ~ (Parser.string("..").token *> expressionP).? ~ 
		(Parser.string(":").token *> stmtRecP))
		.map {
			case ((min,Some(max)),Some(stmt)) => RangeCase(min,max,stmt)
			case ((min,None),Some(stmt)) => SimpleCase(min,stmt)
		}
	}

	def caseStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] = {
		(
			(expressionP.between(stringTokenP("CASE"), stringTokenP("OF"))) ~
			(caseAlternativeP(stmtRecP).backtrack.rep0 ~ (stringTokenP("ELSE") *> stmtRecP).?)
			<* stringTokenP("END")
		)
		.map {
			case (expr, (cases, Some(stmt))) => CaseStmt(expr,cases,stmt)
			case (expr, (cases, None)) => CaseStmt(expr,cases,None)
		}
	}

	def statementP: Parser0[Option[Statement]] = {
		(caseStmtP(Parser.defer0(statementP)) | loopStmtP(Parser.defer0(statementP)) | forEachStmtP(Parser.defer0(statementP)) | forStmtP(Parser.defer0(statementP)) |
		repeatStmtP(Parser.defer0(statementP)) | whileStmtP(Parser.defer0(statementP)) | exitP | returnP |
		ifStmtP(Parser.defer0(statementP)) | readShortIntStmtP | readCharStmtP | readIntStmtP | 
		readLongIntStmtP | readRealStmtP |readLongRealStmtP | assignmentStmtP.backtrack |
		 writeStmtP | procedureCallStmtP.backtrack).?
	}
}