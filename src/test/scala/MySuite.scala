import oberonParser.*
import oberonAST.*

class ParserTestSuite extends munit.FunSuite {
	test("Qualified identifier test") {
			val qualTest1 = OberonParser.qualifiedNameP.parse("myModule::myIdentName123 bla blabla asdna")
			qualTest1 match {
				case Right((_, parsed)) => assertEquals(parsed, "myModule::myIdentName123")
				case _ => fail("Qualified identifier incorrectly parsed.")
			}

			val qualTest2 = OberonParser.qualifiedNameP.parse("1myModule::myIdentName123 bla blabla asdna")
			qualTest2 match {
				case Right(_) => fail("Identifier cannot begin with number")
				case Left(_) => assert(true)
			}
	}

	test("Numbers test") {
		val epsilon = 0.00001
		val eq = (x: Double, y: Double) => (x-y).abs <= epsilon

		val realTest1 = OberonParser.numberP.parse("123.4567")
		realTest1 match {
			case Right(_, RealValue(x)) => assert(eq(x, 123.4567))
			case _ => fail("Failed to parse real number 123.4567")
		}

		val realTest2 = OberonParser.numberP.parse("123456.")
		realTest2 match {
			case Right(_, RealValue(x)) => assert(eq(x, 123456))
			case _ => fail("Failed to parse real number 123456.")
		}

		val intTest1 = OberonParser.numberP.parse("9876 blabla")
		intTest1 match {
			case Right(_, IntValue(n)) => assertEquals(n, 9876)
			case _ => fail("Failed to parse integer 9876")
		}
	}

	test("String test") {
		val stringTest1 = OberonParser.quoteStringP.parse("\"uma string de teste\"abcdef")
		stringTest1 match {
			case Right(_, StringValue(s)) => assertEquals(s, "uma string de teste")
			case _ => fail("Failed to parse string.")
		}
	}
	test("Expression test") {
		val exprTest1 = OberonParser.expressionP.parse("c + j + k")
	}
}
