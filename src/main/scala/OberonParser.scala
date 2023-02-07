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

	implicit class Parser0Ops[A](p: Parser0[A]) {
		import OberonParser.*

		def betweenParen: Parser0[A] = p.between(charTokenP('('), charTokenP(')')) 
		def betweenBrackets: Parser0[A] = p.between(charTokenP('['), charTokenP(']')) 
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
		(identifierP ~ (charTokenP('*').map(x => "*") | Parser.pure("")))
		.map((ident, x) => ident + x).token

	val qualifiedNameHelperP: Parser[String] =
		(identifierP <* stringTokenP("::")).map(a => a + "::").backtrack

	val qualifiedNameP: Parser[String] = 
		((qualifiedNameHelperP.backtrack | Parser.pure("")).with1 ~ identifierP)
		.map((a, b) => a + b).token

	private def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

	def unsignedRealP: Parser[RealValue] = 
		(digit.rep.map(nonEmptyListToInt) ~ (charTokenP('.') *>
		digit.rep0.map(x => "0." + x.mkString)).map(x => x.toDouble))
		.map((intPart, fracPart) => intPart.toDouble + fracPart)
		.map(RealValue.apply)

	def signP: Parser[UnaryArithOperator] =
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
		.surroundedBy(charTokenP('"'))
		.map(StringValue.apply).token

	def charP: Parser[CharValue] = 
		alpha.surroundedBy(charTokenP('\''))
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

	def assignmentStmtP: Parser[Statement] =
		((designatorP(expressionP) <* stringTokenP(":=")) ~ expressionP)
		.map((a, b) =>  AssignmentStmt(designatorHelperToDesignator(a),b))
	
	def writeStmtP: Parser[Statement] =
		(stringTokenP("write") *> expressionP)
		.map(x => WriteStmt(x))

	def designatorHelperToString(x : DesignatorHelper): String = x match {
		case DesignatorHelper(name, Nil) => name 
		case DesignatorHelper(name, selectors) => name + selectors.toString
	}

	def procedureCallStmtP: Parser[Statement] =
		(qualifiedNameP ~ exprListP(expressionP).?.betweenParen)
		.map{ (x,y) => (x,y) match { 
				case (x,None) => ProcedureCallStmt(x,Nil)
				case (x,Some(listinha)) => ProcedureCallStmt(x,listinha)
			}
		}
	
	def readCharStmtP: Parser[Statement] =
		(stringTokenP("readChar") *> designatorP(expressionP).betweenParen)
		.map(x => ReadCharStmt(designatorHelperToString(x)))

	def readRealStmtP: Parser[Statement] =
		(stringTokenP("readReal") *> designatorP(expressionP).betweenParen)
		.map(x => ReadRealStmt(designatorHelperToString(x)))

	def readLongRealStmtP: Parser[Statement] =
		(stringTokenP("readLongReal") *> designatorP(expressionP).betweenParen)
		.map(x => ReadLongRealStmt(designatorHelperToString(x)))

	def readIntStmtP: Parser[Statement] =
		(stringTokenP("readInt") *> designatorP(expressionP).betweenParen)
		.map(x => ReadIntStmt(designatorHelperToString(x)))

	def readLongIntStmtP: Parser[Statement] =
		(stringTokenP("readLongInt") *> designatorP(expressionP).betweenParen)
		.map(x => ReadLongIntStmt(designatorHelperToString(x)))

	def readShortIntStmtP: Parser[Statement] =
		(stringTokenP("readShortInt") *> designatorP(expressionP).betweenParen)
		.map(x => ReadShortIntStmt(designatorHelperToString(x)))

	def ifStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		((stringTokenP("IF") *> expressionP <* stringTokenP("THEN")) ~ sequenceStmtP(stmtRecP) ~ 
		((stringTokenP("ELSEIF") *> expressionP <* stringTokenP("THEN")) ~ sequenceStmtP(stmtRecP)).rep0 ~ 
		(stringTokenP("ELSE") *> sequenceStmtP(stmtRecP)).? <* stringTokenP("END"))
		.map{ 
			case (((expr,statement1),(head1,head2)::listElseIf),statement2) => {
				val foldedList = listElseIf.foldLeft(List(ElseIfStmt(head1,head2))){ 
					case (x,(acc1,acc2)) => (x :+ ElseIfStmt(acc1,acc2)) 
				}
				IfElseIfStmt(expr,statement1,foldedList,statement2)
			}
			case((((expr,statement1),Nil),statement2)) => IfElseStmt(expr,statement1,statement2)
		}

	def sequenceStmtP(stmtRecP: Parser0[Option[Statement]]): Parser0[Statement] =
		(stmtRecP ~ (stringTokenP(";") *> stmtRecP).rep0)
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

	def whileStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		((stringTokenP("WHILE") *> expressionP <* stringTokenP("DO")) ~ 
		sequenceStmtP(stmtRecP) <* stringTokenP("END"))
		.map((x,y) => WhileStmt(x,y))

	def repeatStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		((stringTokenP("REPEAT") *> sequenceStmtP(stmtRecP) <* stringTokenP("UNTIL")) ~ expressionP)
		.map((x,y) => RepeatUntilStmt(y,x))

	def forStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		((stringTokenP("FOR") *> sequenceStmtP(stmtRecP) <* stringTokenP("TO")) ~
		expressionP ~(stringTokenP("DO") *> sequenceStmtP(stmtRecP) <* stringTokenP("END")))
		.map{case ((x,y),z) => ForStmt(x,y,z)}

	def forEachStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		((stringTokenP("FOREACH") *> identifierP <* stringTokenP("IN")) ~ expressionP ~ 
		sequenceStmtP(stmtRecP) <* stringTokenP("END"))
		.map{case ((x,y),z) => ForEachStmt(x,y,z)}

	def loopStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		(stringTokenP("LOOP") *> sequenceStmtP(stmtRecP) <* stringTokenP("END"))
		.map(statement => LoopStmt(statement))

	def returnP: Parser[Statement] =
		(stringTokenP("RETURN") *> expressionP)
		.map((expression) => ReturnStmt(expression))

	def exitP: Parser[Statement] =
		(stringTokenP("EXIT"))
		.map(_ => ExitStmt())

	def labelP: Parser[Expression] =
		decIntegerP | quoteStringP | qualifiedNameP.map(VarExpression.apply)

	def caseAlternativeP(stmtRecP: Parser0[Option[Statement]]): Parser[CaseAlternative] =
		((labelP ~ (stringTokenP("..") *> labelP).? <* charTokenP(':')) ~ sequenceStmtP(stmtRecP))
		.map {
			case ((label, None), stmts) => SimpleCase(label, stmts)
			case ((min, Some(max)), stmts) => RangeCase(min, max, stmts)
		}

	def caseStmtP(stmtRecP: Parser0[Option[Statement]]): Parser[Statement] =
		((expressionP.between(stringTokenP("CASE"), stringTokenP("OF"))) ~
		(caseAlternativeP(stmtRecP).backtrack.repSep(charTokenP('|')) ~
		(stringTokenP("ELSE") *> sequenceStmtP(stmtRecP)).?) <* stringTokenP("END"))
		.map {
			case (expr, (cases, stmt)) => CaseStmt(expr,cases.toList, stmt)
		}

	def statementP: Parser0[Option[Statement]] = 
		(caseStmtP(Parser.defer0(statementP)) | loopStmtP(Parser.defer0(statementP)) |
		forEachStmtP(Parser.defer0(statementP)) | forStmtP(Parser.defer0(statementP)) |
		repeatStmtP(Parser.defer0(statementP)) | whileStmtP(Parser.defer0(statementP)) | exitP |
		returnP | ifStmtP(Parser.defer0(statementP)) | readShortIntStmtP | readCharStmtP |
		readIntStmtP | readLongIntStmtP | readRealStmtP |readLongRealStmtP |
		assignmentStmtP.backtrack | writeStmtP | procedureCallStmtP.backtrack).?

	def userTypeP(oberonTypeRecP: Parser[Type]): Parser[Type] =
		arrayTypeP(oberonTypeRecP) | recordTypeP(oberonTypeRecP) | pointerTypeP(oberonTypeRecP)
	
	def pointerTypeP(oberonTypeRecP: Parser[Type]): Parser[Type] =
		(stringTokenP("POINTER") *> (stringTokenP("TO") *> oberonTypeRecP))
		.map (x => PointerType(x))

	def recordTypeP(oberonTypeRecP: Parser[Type]): Parser[Type] =
		(stringTokenP("RECORD") *> varDeclarationP(oberonTypeRecP).rep0 <* stringTokenP("END"))
		.map(x => RecordType(x))

	def arrayTypeP(oberonTypeRecP: Parser[Type]): Parser[Type] =
		((stringTokenP("ARRAY") *> unsignedDecIntegerP.token <* stringTokenP("OF")) ~ oberonTypeRecP)
		.map{case (IntValue(x),y) => ArrayType(x,y)}

	def varDeclarationP(oberonTypeRecP: Parser[Type]): Parser[VariableDeclaration] = 
		(identifierDefP ~ (stringTokenP(",") *> identifierDefP).rep0 ~ 
		(stringTokenP(":") *> oberonTypeRecP <* stringTokenP(";"))).backtrack
		.map { case ((x,y),z) => 
				val ids = y.foldLeft(x) {
					case (x1,x2) => x1+","+x2
				} 
				VariableDeclaration(ids,z)
			}

	def oberonTypeP: Parser[Type] =
		userTypeP(Parser.defer(oberonTypeP)) | stringTokenP("INTEGER").map(_=> IntegerType) | 
		stringTokenP("REAL").map(_ => RealType) | stringTokenP("CHAR").map(_ => CharacterType) | 
		stringTokenP("BOOLEAN").map(_ => BooleanType) | stringTokenP("STRING").map(_ => StringType) | 
		nullP.map(_ => NullType) | qualifiedNameP.map(name => ReferenceToUserDefinedType(name))

	def formalsP = (formalArgP ~ (stringTokenP(",") *> formalArgP).rep0).?.betweenParen

	def procedureP: Parser[Procedure] =
		((stringTokenP("PROCEDURE") *> identifierDefP) ~ formalsP ~ 
		((stringTokenP(":") *> oberonTypeP).? <* stringTokenP(";")) ~ 
		(stringTokenP("CONST") *> constantP.rep0).? ~ (stringTokenP("VAR") *> varDeclarationP(oberonTypeP).rep0).? ~
		(stringTokenP("BEGIN") *> sequenceStmtP(statementP) <* stringTokenP("END")) ~ identifierP)
		.map{ case (((((((id,option1),tipo),option2),option3),stmt),endId)) => {
				if(id != endId) 
					throw new Exception(s"Procedure name ($id) doesn't match the end identifier ($endId)")
				val args = option1 match {
					case Some(head,tail) => (head :: tail)
					case None => Nil
				} 
				val constantes = option2 match {
					case Some(listConst) => listConst
					case None => Nil
				} 
				val variaveis = option3 match {
					case Some(listVar) => listVar
					case None => Nil
				}
				Procedure(id,args.flatten,tipo,constantes,variaveis,stmt)
			}
		}
	
	def constantP: Parser[Constant] =
		(identifierDefP ~ (stringTokenP("=") *> expressionP <* stringTokenP(";"))).backtrack
		.map((x,y) => Constant(x,y))

	def formalArgP: Parser[List[FormalArg]] = 
		parameterByReferenceP | parameterByValueP

	def parameterByReferenceP: Parser[List[FormalArg]] = 
		(stringTokenP("VAR") *> qualifiedNameP ~ (stringTokenP(",") *> 
		(stringTokenP("VAR") *> qualifiedNameP)).rep0 ~ (stringTokenP(":") *> oberonTypeP))
		.map{ case ((x,y),z) => (x :: y).map{x => ParameterByReference(x, z)}
		}

	def parameterByValueP: Parser[List[FormalArg]] = 
		(qualifiedNameP ~ (stringTokenP(",") *> qualifiedNameP).rep0 ~ (stringTokenP(":") *> oberonTypeP))
		.map{ case ((x,y),z) => 
			(x :: y).map{x => ParameterByValue(x, z)}
		}
	
	def importP: Parser[(String, Option[String])] = identifierP ~ (stringTokenP(":=") *> identifierP ).?

	def importListP: Parser[Set[String]] = 
		(stringTokenP("IMPORT") *> importP ~ (stringTokenP(",") *> importP).rep0 <* stringTokenP(";"))
		.map{ case (head,tail) => {
				(head::tail).foldLeft(Set()) {
					case (total,(head1,Some(head2))) => total.++(Set(head1+":="+head2))
					case (total,(head1,None)) => total.++(Set(head1))
				}
			}
		}
	
	// Ultima pergunta para o professor // LEMBRARRRRRRR //
	// perguntar se o correto seria ser oberonType ou userType igual especificado em g4
	def userTypeDeclarationP: Parser[UserDefinedType] = {
		((identifierDefP <* stringTokenP("=")) ~ userTypeP(oberonTypeP))
		.map((x,y) => UserDefinedType(x,y))
	}

	def oberonModuleP: Parser[OberonModule] = 
		((stringTokenP("MODULE") *> identifierP <* stringTokenP(";")) ~ importListP.? ~
		(stringTokenP("TYPE") *> userTypeDeclarationP.rep0).? ~ (stringTokenP("CONST") *> constantP.rep0).? ~ 
		(stringTokenP("VAR") *> varDeclarationP(oberonTypeP).rep0).? ~ procedureP.rep0.? ~
		(stringTokenP("BEGIN") *> sequenceStmtP(statementP) <* stringTokenP("END")).? ~ 
		(stringTokenP("END") *> identifierP <* stringTokenP(".")))
		.map { case (((((((id,option1),option2),option3),option4),option5),stmt),endId) => 
			if(id != endId) 
				throw new Exception(s"Module name ($id) doesn't match the end identifier ($endId)")
			val setModules = option1 match {
				case Some(set) => set
				case None => Set()
			}
			val listUserType = option2 match {
				case Some(list) => list
				case None => Nil 
			}
			val listConst = option3 match {
				case Some(list) => list
				case None => Nil 
			}
			val listVarDeclaration = option4 match {
				case Some(list) => list
				case None => Nil 
			}
			val listProcedure = option5 match {
				case Some(list) => list
				case None => Nil 
			}
			OberonModule(id,setModules,listUserType,listConst,listVarDeclaration,listProcedure,stmt)
		}
}