import oberonParser.*
import cats.implicits.*

class ParserTestSuite extends munit.FunSuite {
  test("Qualified identifier test") {
			val qualTest1 = OberonParser.qualifiedNameP.parse("myModule::myIdentName123* bla blabla asdna")
			qualTest1 match {
				case Right((_, parsed)) => assertEquals(parsed, "myModule::myIdentName123*")
				case _                  => fail("Qualified identifier incorrectly parsed.")
			}

			val qualTest2 = OberonParser.qualifiedNameP.parse("1myModule::myIdentName123 bla blabla asdna")
			qualTest2 match {
				case Right(_) => fail("Identifier cannot begin with number")
				case Left(_) => assert(true)
			}
	}
}
