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

		val exprTest3 = expressionP.parseString("abc.myarray[5]")
		println(exprTest3)

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
			case Left(_) => fail
			case Right(str, value) => assert(value == TimesOperator && str == "5")
		}
		multTest2 match {
			case Left(_) => fail
			case Right(str, value) => assert(value == AndOperator && str == "True")
		}
		multTest3 match {
			case Left(_) => fail
			case Right(str, value) => assert(value == SlashOperator && str == "3")
		}
		multTest4 match {
			case Left(_) => fail
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
}
