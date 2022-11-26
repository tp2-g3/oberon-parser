package oberonParser

import cats.parse.Rfc5234.{sp, alpha, digit, char}
import cats.parse.{Parser, Parser0}
import cats.data.NonEmptyList

sealed trait AddOperator
case object PlusOperator extends AddOperator
case object MinusOperator extends AddOperator
case object OrOperator extends AddOperator

sealed trait MulOperator
case object TimesOperator extends MulOperator
case object SlashOperator extends MulOperator
case object DivOperator extends MulOperator
case object ModOperator extends MulOperator
case object AndOperator extends MulOperator

final case class Identifier(name: String)

sealed trait HexDigit
final case class LetterDigit(letter: Char) extends HexDigit
final case class NumberDigit(number: Char) extends HexDigit

sealed trait ScaleFactorSign
case object ScalePlus extends ScaleFactorSign
case object ScaleMinus extends ScaleFactorSign

final case class ScaleFactor(sign: Option[ScaleFactorSign], number:Int)

sealed trait Number
final case class RealNumber(number: Double, scale: Option[ScaleFactor]) extends Number
final case class IntegerNumber(number: Int) extends Number

object OberonParser {
	private val whitespaceP: Parser[Unit] = Parser.charIn(" \t\r\n").void
	private val whitespacesP: Parser0[Unit] = whitespaceP.rep0.void

	private val addOperatorP: Parser[AddOperator] = 
	Parser.char('+').map(x => PlusOperator) | Parser.char('-').map(x => MinusOperator) |
	Parser.string("OR").map(x => OrOperator)

	private val mulOperatorP: Parser[MulOperator] = 
	Parser.char('*').map(x => TimesOperator) | Parser.char('/').map(x => SlashOperator) |
	Parser.string("DIV").map(x => DivOperator) | Parser.string("MOD").map(x => ModOperator) |
	Parser.char('&').map(x => AndOperator)

	private val identifierP: Parser[Identifier] = (alpha ~ (alpha | digit).rep0).map((x, xs) => x :: xs).
	map(s => Identifier(s.toString))

	private val hexDigitP: Parser[HexDigit] = Parser.charIn("ABCDEF").map(LetterDigit.apply) | 
	digit.map(NumberDigit.apply)

	private def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

	private val scaleFactorP: Parser[ScaleFactor] = (Parser.char('E') *> 
	(Parser.char('+').map(x => Some(ScalePlus)) | Parser.char('-').map(x => Some(ScaleMinus)) |
	Parser.pure(None)) ~ digit.rep.map(nonEmptyListToInt)).map(ScaleFactor.apply)

	private val realHelperP: Parser[((Double, List[Char]), Option[ScaleFactor])] =
	(digit.rep.map(nonEmptyListToInt) <* Parser.char('.')).map(x => x.toDouble) ~
	digit.rep0 ~ (scaleFactorP.map(Some.apply) | Parser.pure(None))

	private def decimalPartOfList(l: List[Char]): Double = {
		val powers = for {
			i <- 1 to l.length
		} yield l(i-1).asDigit.toDouble * scala.math.pow(10, -i)
		powers.foldLeft(0: Double)((acc, x) => acc + x)
	}

	private def realP: Parser[RealNumber] = realHelperP.map{ case ((intPart, l), scale) => 
		RealNumber(intPart + decimalPartOfList(l), scale)
	}

	private def decIntegerP: Parser[IntegerNumber] = {
	digit.rep
	.map(nonEmptyListToInt.apply)
	.map(IntegerNumber.apply)}

	private def hexListToString(l: List[HexDigit]): String = l.toList.foldLeft("") {
		(acc: String, x: HexDigit) => x match {
			case LetterDigit(x: Char) => acc ++ x.toString
			case NumberDigit(x: Char) => acc ++ x.toString
		}
	}

	private def hexIntegerP: Parser[IntegerNumber] =
		(digit ~ hexDigitP.rep0 <* Parser.char('H'))
		.map((x, l) => Integer.parseInt(hexListToString(NumberDigit(x)::l), 16))
		.map(IntegerNumber.apply)

	private def integerP: Parser[IntegerNumber] = hexIntegerP.backtrack | decIntegerP

	private def numberP: Parser[Number] = realP.backtrack | integerP

	private def quoteStringP: Parser[String] = 
		Parser.charsWhile(x => x != '"').surroundedBy(Parser.char('"'))

	private def hexStringP: Parser[String] = 
		(digit ~ hexDigitP.rep0 <* Parser.char('X'))
		.map((x, l) => Integer.parseInt(hexListToString(NumberDigit(x)::l), 16))
		.map(x => x.toChar.toString)

	private def stringP: Parser[String] = quoteStringP.backtrack | hexStringP
}