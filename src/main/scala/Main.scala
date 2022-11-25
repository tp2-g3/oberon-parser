import oberonParser._

@main def hello: Unit = {
	println(OberonParser.integerP.parse("10H"))
}
