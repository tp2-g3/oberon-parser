import oberonParser._

@main def hello: Unit = {
	println(OberonParser.realP.parse("123.789E+1234"))
}
