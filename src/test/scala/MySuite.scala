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
		val exprTest1 = expressionP.parseString("  1 + 2*3")
		exprTest1 match {
			case Right("", expr) => assertEquals
				(expr, AddExpression(IntValue(1), MultExpression(IntValue(2), IntValue(3))))
			case _ => fail("Expression test 1 failed")
		}

		val exprTest2 = expressionP.parseString(" - 1.23 + 5.68 - (4+2) = 5")
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
	test("Statement Test 1"){
		println(statementP.parse("x.y.z :=3"))
	}
	test("Statement Test 2"){
		println(statementP.parse("x.y.z[5] :=3"))
	}
	test("Statement Test 3"){
		println(statementP.parse("z^ :=3"))
	}
	test("Statement Test 4"){
		println(statementP.parse("a.z^ :=3"))
	}
	test("Statement Test 5"){
		println(statementP.parse("a :=3"))
	}
	test("Statement Test 6"){
		println(statementP.parse("z[i+10] :=3"))
	}
	test("Statement Test 7"){
		println(statementP.parse("write(x+5)"))
	}
	test("Statement Test 8"){
		println(statementP.parse("seilaman(x,5)"))
	}
	test("Statement Test 9"){
		println(statementP.parse("seilaman()"))
	}
	test("Statement Test 10"){
		println(statementP.parse("IF 1+3=4 THEN a:=3 END"))
	}
	test("Statement Test 11"){
		println(statementP.parse("IF 1+3=4 THEN a:=3 ELSE a:=1 END"))
	}
	test("Statement Test 12"){
		println(statementP.parse("IF 1+3=a THEN a:=3 ELSEIF 1+3=4 THEN a:=2 ELSE a:=1 END"))
	}
	test("Statement Test 13"){
		println(statementP.parse("IF 1+3=3 THEN a:=3 ELSEIF 1+3=4 THEN a:=2 END"))
	}
	test("Statement Test 14"){
		println(statementP.parse("IF 1+3=3 THEN a:=3 ELSEIF 1+3=4 THEN a:=2 ELSEIF 1+3=5 THEN a:=5 END"))
	}
	test("Statement Test 15"){
		println(statementP.parse("IF 1+3=3 THEN IF 1+i=(j-2) THEN a := 1 ; b := 3 ; c := a+b ; write(c) END ELSEIF 1+3=4 THEN a:=2 ELSEIF 1+3=5 THEN a:=5 END"))
	}
	test("Statement Test 16"){
		println(statementP.parse("readLongReal(x)"))
	}
	test("Statement Test 17"){
		println(statementP.parse("readReal(x)"))
	}
	test("Statement Test 18"){
		println(statementP.parse("readLongInt(x)"))
	}
	test("Statement Test 19"){
		println(statementP.parse("readInt(x)"))
	}
	test("Statement Test 20"){
		println(statementP.parse("readChar(x)"))
	}
	test("Statement Test 21"){
		println(statementP.parse("readShortInt(x)"))
	}
	test("Statement Test 22"){
		println(statementP.parse("WHILE i+1 = j DO readInt(x) ; x := 10 ; c := 2*x END"))
	}
	test("Statement Test 23"){
		println(statementP.parse("WHILE i+1 = j DO IF 2*c = x THEN readInt(x) ; x := 10 ; c := 2*x END END"))
	}
}
