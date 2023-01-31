import oberonParser.OberonParser.*
import oberonParser.ParserSyntax.*
import oberonAST.*

class ParserTestSuite extends munit.FunSuite {
	test("Qualified identifier test") {
			val qualTest1 = qualifiedNameP.parseString("myModule::myIdentName123 blabla1")
			qualTest1 match {
				case Right(("blabla1", parsed)) => assertEquals(parsed, "myModule::myIdentName123")
				case _ => fail("Qualified identifier incorrectly parsed.")
			}

			val qualTest2 = qualifiedNameP.parseString("1myModule::myIdentName123 bla blabla asdna")
			qualTest2 match {
				case Right(_) => fail("Identifier cannot begin with number")
				case Left(_) => assert(true)
			}
	}

	test("Numbers test") {
		val epsilon = 0.00001
		val eq = (x: Double, y: Double) => (x-y).abs <= epsilon

		val realTest1 = numberP.parseString("123.4567")
		realTest1 match {
			case Right("", RealValue(x)) => assert(eq(x, 123.4567))
			case _ => fail("Failed to parse real number 123.4567")
		}

		val realTest2 = numberP.parseString("123456.")
		realTest2 match {
			case Right(_, RealValue(x)) => assert(eq(x, 123456))
			case _ => fail("Failed to parse real number 123456.")
		}

		val intTest1 = numberP.parseString("9876 blabla")
		intTest1 match {
			case Right("blabla", IntValue(n)) => assertEquals(n, 9876)
			case _ => fail("Failed to parse integer 9876")
		}
	}

	test("String test") {
		val stringTest1 = quoteStringP.parseString("\"uma string de teste\"abcdef")
		stringTest1 match {
			case Right("abcdef", StringValue(s)) => assertEquals(s, "uma string de teste")
			case _ => fail("Failed to parse string.")
		}
	}
	test("Expression test") {
		val exprTest1 = expressionP.parseString("    /* asdmasdk */ 1 + 2*3  /* adasd */")
		exprTest1 match {
			case Right("", expr) => assertEquals
				(expr, AddExpression(IntValue(1), MultExpression(IntValue(2), IntValue(3))))
			case _ => fail("Expression test 1 failed")
		}

		val exprTest2 = expressionP.parseString(" - 1.23 /* asd */ + 5.68 - (4+2) = 5")
		exprTest2 match {
			case Right(str, _) => assert(str == "")
			case _ => fail("Expression test 2 failed")
		}

		val exprTest3 = expressionP.parseString("a.b.c^")
		
		val exprTest4 = expressionP.parseString("1 >= 2")
		exprTest4 match {
			case Right("", value) => assert(value == GTEExpression(IntValue(1), IntValue(2)))
			case _ => fail("Failed expression 4 test")
		}
	}

	test("Bool test") {
		val boolTest1 = boolP.parseString("True = False")
		boolTest1 match {
			case Right("= False", value) => assert(value == BoolValue(true))
			case _ => fail("Bool test 1 failed to recognize True")
		}

		val boolTest2 = boolP.parseString("False = a + b")
		boolTest2 match {
			case Right("= a + b", value) => assert(value == BoolValue(false))
			case _ => fail("Bool test 2 failed to recognize False")
		}

		val boolTest3 = boolP.parseString("FALSE = a + b")
		boolTest3 match {
			case Left(_) => assert(true)
			case Right(_) => fail("Bool parser misrecognized FALSE as boolean")
		}

		val boolTest4 = boolP.parseString("TRUE = a + b")
		boolTest4 match {
			case Left(_) => assert(true)
			case Right(_) => fail("Bool parser misrecognized TRUE as boolean")
		}
	}

	test("Mult test") {
		val multTest1 = multP.parseString("* 5")
		val multTest2 = multP.parseString("&& True")
		val multTest3 = multP.parseString("/ 3")
		val multTest4 = multP.parseString("MOD 6 + 5")

		multTest1 match {
			case Left(_) => fail("Mult test 1 failed")
			case Right(str, value) => assert(value == TimesOperator && str == "5")
		}
		multTest2 match {
			case Left(_) => fail("Mult test 2 failed")
			case Right(str, value) => assert(value == AndOperator && str == "True")
		}
		multTest3 match {
			case Left(_) => fail("Mult test 3 failed")
			case Right(str, value) => assert(value == SlashOperator && str == "3")
		}
		multTest4 match {
			case Left(_) => fail("Mult test 4 failed")
			case Right(str, value) => assert(value == ModOperator && str == "6 + 5")
		}
	}

	test("Add test") {
		val addTest1 = addP.parseString("+")
		addTest1 match {
			case Right(str, value) => assert(value == PlusOperator && str == "")
			case Left(_) => fail("Failed to parse add test 1")
		}

		val addTest2 = addP.parseString("-")
		addTest2 match {
			case Right(str, value) => assert(value == MinusOperator && str == "")
			case Left(_) => fail("Failed to parse add test 2")
		}

		val addTest3 = addP.parseString("||")
		addTest3 match {
			case Right(str, value) => assert(value == OrOperator && str == "")
			case Left(_) => fail("Failed to parse add test 3")
		}
	}

	test("relationP test") {
		val relationTest1 = relationP.parseString("= 5")
		val relationTest2 = relationP.parseString("# something")
		val relationTest3 = relationP.parseString("<= 2")
		val relationTest4 = relationP.parseString("< 4")
		val relationTest5 = relationP.parseString(">= 1")
		val relationTest6 = relationP.parseString("> 10")

		relationTest1 match {
			case Right(str, value) => assert(value == EQOperator && str == "5")
			case Left(_) => fail("Failed to parse add test 1")
		}

		relationTest2 match {
			case Right(str, value) => assert(value == NEQOperator && str == "something")
			case Left(_) => fail("Failed to parse add test 2")
		}

		relationTest3 match {
			case Right(str, value) => assert(value == LTEOperator && str == "2")
			case Left(_) => fail("Failed to parse add test 3")
		}

		relationTest4 match {
			case Right(str, value) => assert(value == LTOperator && str == "4")
			case Left(_) => fail("Failed to parse add test 4")
		}

		relationTest5 match {
			case Right(str, value) => assert(value == GTEOperator && str == "1")
			case Left(_) => fail("Failed to parse add test 5")
		}

		relationTest6 match {
			case Right(str, value) => assert(value == GTOperator && str == "10")
			case Left(_) => fail("Failed to parse add test 6")
		}
	}

	test("nullP test") {
		val nullTest1 = nullP.parseString("NIL")
		val nullTest2 = nullP.parseString("NIL ")

		nullTest1 match {
			case Right(str, value) => assert(value == NullValue && str == "")
			case Left(_) => fail("Failed to parse null test 1")
		}

		nullTest2 match {
			case Right(str, value) => assert(value == NullValue && str == "")
			case Left(_) => fail("Failed to parse null test 2")
		}
	}

	test("realP test") {
		val realTest1 = realP.parseString("+3.14")
		val realTest2 = realP.parseString("-2.718")
		val realTest3 = realP.parseString("0.0")
		val realTest4 = realP.parseString("1.234")
		val realTest5 = realP.parseString("-0.5")

		realTest1 match {
			case Right(str, value) => assert(value == RealValue(3.14) && str == "")
			case Left(_) => fail("Failed to parse real test 1")
		}

		realTest2 match {
			case Right(str, value) => assert(value == RealValue(-2.718) && str == "")
			case Left(_) => fail("Failed to parse real test 2")
		}

		realTest3 match {
			case Right(str, value) => assert(value == RealValue(0.0) && str == "")
			case Left(_) => fail("Failed to parse real test 3")
		}

		realTest4 match {
			case Right(str, value) => assert(value == RealValue(1.234) && str == "")
			case Left(_) => fail("Failed to parse real test 4")
		}

		realTest5 match {
			case Right(str, value) => assert(value == RealValue(-0.5) && str == "")
			case Left(_) => fail("Failed to parse real test 5")
		}
	}

	test("charP test") {
		val charTest1 = charP.parseString("'a'")
		val charTest2 = charP.parseString("'b'")
		val charTest3 = charP.parseString("'z'")

		charTest1 match {
			case Right(str, value) => assert(value == CharValue('a'))
			case Left(_) => fail("Failed to parse char test 1")
		}

		charTest2 match {
			case Right(str, value) => assert(value == CharValue('b'))
			case Left(_) => fail("Failed to parse char test 2")
		}

		charTest3 match {
			case Right(str, value) => assert(value == CharValue('z'))
			case Left(_) => fail("Failed to parse char test 3")
		}
	}

	test("Statement Test Assignment"){
		val test1 = statementP.parse("x.y.z := 3") 
		val test2 = statementP.parse("z^ :=3")
		val test3 = statementP.parse("a.z^ :=3")

		test1 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(AssignmentStmt(RecordAssignment(FieldAccessExpression(VarExpression(x),y),z),IntValue(3)))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		test2 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(AssignmentStmt(PointerAssignment(z),IntValue(3)))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		test3 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(AssignmentStmt(ComplexPointerAssignment(FieldAccessExpression(VarExpression(a),z)),IntValue(3)))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		val test4 = statementP.parse("a :=3") 
		val test5 = statementP.parse("z[i+10] :=3")
		val test6 = statementP.parse("z := 3+y")

		test4 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(AssignmentStmt(VarAssignment(a),IntValue(3)))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test5 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(AssignmentStmt(ArrayAssignment(VarExpression(z),AddExpression(VarExpression(i),IntValue(10))),IntValue(3)))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test6 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(AssignmentStmt(VarAssignment(z),AddExpression(IntValue(3),VarExpression(y))))) =>  true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement write Test"){
		statementP.parse("write(x+5)") match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(WriteStmt(AddExpression(VarExpression(x),IntValue(5))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement procedure call Test"){
		val test1 = statementP.parse("seilaman(x,5)")
		val test2 = statementP.parse("seilaman()")

		test1 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ProcedureCallStmt(seilaman,List(VarExpression(x), IntValue(5))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test2 match {
			case Left(_) => fail("Statement test failed")
			case Right(str,Some(ProcedureCallStmt(seilaman,List()))) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement ifElse and ifElseIf Test"){
		val test1 = statementP.parse("IF 1+3=4 THEN a:=3 END")
		val test2 = statementP.parse("IF 1+3=4 THEN a:=3 ELSE a:=1 END")
		val test3 = statementP.parse("IF 1+3=a THEN a:=3 ELSEIF 1+3=4 THEN a:=2 ELSE a:=1 END")

		test1 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(IfElseStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(4)),
			SequenceStmt(List(AssignmentStmt(VarAssignment(a),IntValue(3)))),None))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test2 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(IfElseStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(4)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(3)))),
			Some(SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(1)))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test3 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(IfElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),VarExpression(a)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(3)))),
			List(ElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(4)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(2)))))),
			Some(SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(1)))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		
		val test4 = statementP.parse("IF 1+3=3 THEN a:=3 ELSEIF 1+3=4 THEN a:=2 END")
		val test5 = statementP.parse("IF 1+3=3 THEN a:=3 ELSEIF 1+3=4 THEN a:=2 ELSEIF 1+3=5 THEN a:=5 END")

		test4 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(IfElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(3)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(3)))),
			List(ElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(4)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(2)))))),None))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test5 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(IfElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(3))
			,SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(3)))),
			List(ElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(4)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(2))))), 
			ElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(5)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(5)))))),None))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		val test6 = statementP.parse("IF 1+3=3 THEN IF 1+i=(j-2) THEN a := 1 ; b := 3 ; c := a+b ; write(c) END ELSEIF 1+3=4 THEN a:=2 ELSEIF 1+3=5 THEN a:=5 END")
		
		test6 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(IfElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(3)),
			SequenceStmt(List(IfElseStmt(EQExpression(AddExpression(IntValue(1),VarExpression(i)),SubExpression(VarExpression(j),IntValue(2))),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(1)), AssignmentStmt(VarAssignment("b"),IntValue(3)), 
			AssignmentStmt(VarAssignment("c"),AddExpression(VarExpression("a"),VarExpression("b"))), WriteStmt(VarExpression(c)))),None))),
			List(ElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(4)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(2))))), 
			ElseIfStmt(EQExpression(AddExpression(IntValue(1),IntValue(3)),IntValue(5)),
			SequenceStmt(List(AssignmentStmt(VarAssignment("a"),IntValue(5)))))),None))) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement Read's Test"){
		val test1 = statementP.parse("readLongReal(x)")
		val test2 = statementP.parse("readReal(x)")
		val test3 = statementP.parse("readLongInt(x)")

		test1 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReadLongRealStmt(x))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test2 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReadRealStmt(x))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test3 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReadLongIntStmt(x))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		val test4 = statementP.parse("readInt(x)")
		val test5 = statementP.parse("readChar(x)")
		val test6 = statementP.parse("readShortInt(x)") 

		test4 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReadIntStmt(x))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test5 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReadCharStmt(x))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test6 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReadShortIntStmt(x))) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement Loop's Test"){
		val test1 = statementP.parse("WHILE i+1 = j DO readInt(x) ; x := 10 ; c := 2*x END")
		val test2 = statementP.parse("WHILE i+1 = j DO IF 2*c = x THEN readInt(x) ; x := 10 ; c := 2*x END END")
		val test3 = statementP.parse("REPEAT readInt(x) ; x := 10 ; c := 2*x UNTIL i+1 = j")

		test1 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(WhileStmt(EQExpression(AddExpression(VarExpression("i"),IntValue(1)),VarExpression("j")),
			SequenceStmt(List(ReadIntStmt("x"), AssignmentStmt(VarAssignment("x"),IntValue(10)), 
			AssignmentStmt(VarAssignment("c"),MultExpression(IntValue(2),VarExpression("x")))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test2 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(WhileStmt(EQExpression(AddExpression(VarExpression("i"),IntValue(1)),VarExpression("j")),
			SequenceStmt(List(IfElseStmt(EQExpression(MultExpression(IntValue(2),VarExpression("c")),VarExpression("x")),
			SequenceStmt(List(ReadIntStmt("x"), AssignmentStmt(VarAssignment("x"),IntValue(10)), 
			AssignmentStmt(VarAssignment("c"),MultExpression(IntValue(2),VarExpression("x"))))),None)))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test3 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(RepeatUntilStmt(EQExpression(AddExpression(VarExpression("i"),IntValue(1)),VarExpression("j")),
			SequenceStmt(List(ReadIntStmt("x"), AssignmentStmt(VarAssignment("x"),IntValue(10)), 
			AssignmentStmt(VarAssignment("c"),MultExpression(IntValue(2),VarExpression("x")))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		val test4 = statementP.parse("REPEAT IF 2*c = x THEN readInt(x) ; x := 10 ; c := 2*x END UNTIL i+1 = j")
		val test5 = statementP.parse("FOR x:=1 ; y:=2 TO x<y DO write(x) ; write(y) ; x:=x+1 END")
		val test6 = statementP.parse("WHILE x<y DO write(x) ; write(y) ; x:=x+1 END") 

		test4 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(RepeatUntilStmt(EQExpression(AddExpression(VarExpression("i"),IntValue(1)),VarExpression("j")),
			SequenceStmt(List(IfElseStmt(EQExpression(MultExpression(IntValue(2),VarExpression("c")),VarExpression("x")),
			SequenceStmt(List(ReadIntStmt("x"), AssignmentStmt(VarAssignment("x"),IntValue(10)), 
			AssignmentStmt(VarAssignment("c"),MultExpression(IntValue(2),VarExpression("x"))))),None)))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test5 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ForStmt(SequenceStmt(List(AssignmentStmt(VarAssignment("x"),IntValue(1)), 
			AssignmentStmt(VarAssignment("y"),IntValue(2)))),LTExpression(VarExpression("x"),VarExpression("y")),
			SequenceStmt(List(WriteStmt(VarExpression("x")), WriteStmt(VarExpression("y")), 
			AssignmentStmt(VarAssignment("x"),AddExpression(VarExpression("x"),IntValue(1)))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
		test6 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(WhileStmt(LTExpression(VarExpression("x"),VarExpression("y")),SequenceStmt(List(WriteStmt(VarExpression("x")), 
			WriteStmt(VarExpression("y")), AssignmentStmt(VarAssignment("x"),AddExpression(VarExpression("x"),IntValue(1)))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}

		val test7 = statementP.parse("LOOP write(x) ; write(y) ; END")
		
		test7 match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(LoopStmt(SequenceStmt(List(WriteStmt(VarExpression(x)), WriteStmt(VarExpression(y))))))) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement Return Test"){
		statementP.parse("RETURN x") match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ReturnStmt(VarExpression(x)))) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Statement Exit Test"){
		statementP.parse("EXIT") match {
			case Left(_) => fail("Statement test failed")
			case Right("",Some(ExitStmt())) => true
			case Right(_,_) => fail("Statement test failed")
		}
	}
	test("Case statement"){
		val result1 = Some(
			CaseStmt
			(
				VarExpression("xs"), 
				List(RangeCase(IntValue(1), IntValue(20),
				SequenceStmt(List(AssignmentStmt(VarAssignment("x"), IntValue(10))))),
				SimpleCase(IntValue(2), 
				SequenceStmt(List(AssignmentStmt(VarAssignment("x"), IntValue(20)))))),
				Some(SequenceStmt(List(AssignmentStmt(VarAssignment("x"), IntValue(0)))))
			)
		)

		statementP.parse("CASE xs OF 1..20: x:= 10 | 2: x:= 20 ELSE x:= 0 END") match {
			case Right("", result) => assert(result == result1)
			case _ => fail("Case statement test 1 failed.")
		}
	}
	test("Module tests") {
		val module1 = oberonModuleP.parseString("""
		MODULE SimpleModule;

		VAR
		x : INTEGER;
		y : INTEGER;
		
		BEGIN
		x := 5;
		y := 100;
		WHILE(x < y) DO
		x := x * x
		END;
		write(x)
		END

		END SimpleModule.
		""")

		module1 match {
			case Right("", parsed) => assertEquals(parsed,
			OberonModule("SimpleModule",Set(),List(),List(),List(VariableDeclaration("x",IntegerType),
			 VariableDeclaration("y",IntegerType)),List(),Some(SequenceStmt(List(AssignmentStmt(VarAssignment("x")
			 ,IntValue(5)), AssignmentStmt(VarAssignment("y"),IntValue(100)), 
			 WhileStmt(LTExpression(VarExpression("x"),VarExpression("y")),
			 SequenceStmt(List(AssignmentStmt(VarAssignment("x"),MultExpression(VarExpression("x"),
			 VarExpression("x")))))), WriteStmt(VarExpression("x")))))))
			case _ => fail("Module 1 parsed incorrectly.")
		}

		val module2 = oberonModuleP.parseString("""
		MODULE bee1013;

		VAR
			a: INTEGER;
			b: INTEGER;
			c: INTEGER;
			answer: INTEGER;

		PROCEDURE abs(a : INTEGER) : INTEGER;
			BEGIN
				IF (a < 0) THEN
					a := a * (-1)
				END;
				RETURN a
			END abs

		PROCEDURE maxValue(a, b : INTEGER) : INTEGER;
			BEGIN
				RETURN (a + b + abs(a - b)) /2
			END maxValue

		BEGIN
			a:= 7;
			b:= 14;
			c:= 106;

			answer := maxValue(maxValue(a, b), c)

		END

		END bee1013.
		""")


		module2 match {
			case Right("", parsed) => assertEquals(parsed,
			OberonModule("bee1013",Set(),List(),List(),List(VariableDeclaration("a",IntegerType),
			 VariableDeclaration("b",IntegerType), VariableDeclaration("c",IntegerType),
			  VariableDeclaration("answer",IntegerType)),List(Procedure("abs",
			  List(ParameterByValue("a",IntegerType)),Some(IntegerType),List(),List(),
			  SequenceStmt(List(IfElseStmt(LTExpression(VarExpression("a"),IntValue(0)),
			  SequenceStmt(List(AssignmentStmt(VarAssignment("a"),MultExpression(VarExpression("a"),IntValue(-1))))),
			  None), ReturnStmt(VarExpression("a"))))), Procedure("maxValue",List(ParameterByValue("a",
			  IntegerType), ParameterByValue("b",IntegerType)),Some(IntegerType),List(),List(),
			  SequenceStmt(List(ReturnStmt(DivExpression(AddExpression(AddExpression(VarExpression("a"),
			  VarExpression("b")),FunctionCallExpression("abs",List(SubExpression(VarExpression("a"),
			  VarExpression("b"))))),IntValue(2))))))),Some(SequenceStmt(List(AssignmentStmt
			  (VarAssignment("a"),IntValue(7)), AssignmentStmt(VarAssignment("b"),IntValue(14)),
			   AssignmentStmt(VarAssignment("c"),IntValue(106)), AssignmentStmt
			   (VarAssignment("answer"),FunctionCallExpression("maxValue",
			   List(FunctionCallExpression("maxValue",List(VarExpression("a"),
			VarExpression("b"))), VarExpression("c"))))))))
			)
			case _ => fail("Module 2 parsed incorrectly")
		}
	}
}
