import oberonParser.OberonParser.*
import oberonParser.ParserSyntax.*
import oberonAST.*

class ParserTestSuite extends munit.FunSuite {
	test("Qualified identifier test") {
			val qualTest1 = qualifiedNameP.parseString("myModule::myIdentName123 bla blabla asdna")
			qualTest1 match {
				case Right((_, parsed)) => assertEquals(parsed, "myModule::myIdentName123")
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
			case Right(_, RealValue(x)) => assert(eq(x, 123.4567))
			case _ => fail("Failed to parse real number 123.4567")
		}

		val realTest2 = numberP.parseString("123456.")
		realTest2 match {
			case Right(_, RealValue(x)) => assert(eq(x, 123456))
			case _ => fail("Failed to parse real number 123456.")
		}

		val intTest1 = numberP.parseString("9876 blabla")
		intTest1 match {
			case Right(_, IntValue(n)) => assertEquals(n, 9876)
			case _ => fail("Failed to parse integer 9876")
		}
	}

	test("String test") {
		val stringTest1 = quoteStringP.parseString("\"uma string de teste\"abcdef")
		stringTest1 match {
			case Right(_, StringValue(s)) => assertEquals(s, "uma string de teste")
			case _ => fail("Failed to parse string.")
		}
	}
	test("Expression test") {
		val exprTest1 = expressionP.parseString("  1 + 2*3")
		exprTest1 match {
			case Right(_, expr) => assertEquals
				(expr, AddExpression(IntValue(1), MultExpression(IntValue(2), IntValue(3))))
			case _ => fail
		}

		val exprTest2 = expressionP.parseString(" - 1.23 + 5.68 - (4+2) = 5")
		exprTest2 match {
			case Right(_) => assert(true)
			case _ => fail
		}

		val exprTest3 = expressionP.parseString("GET(a)")
		println(exprTest3)
	}
}
