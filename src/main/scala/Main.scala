import oberonParser._

@main def hello: Unit = {
	println(OberonParser.numberP.parse("100H"))
}
