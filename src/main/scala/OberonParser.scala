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
final case class NumberDigit(number: Int) extends HexDigit

sealed trait ScaleFactorSign
case object ScalePlus extends ScaleFactorSign
case object ScaleMinus extends ScaleFactorSign

final case class ScaleFactor(sign: Option[ScaleFactorSign], number:Int)

final case class Real(number: Double, scale: Option[ScaleFactor])

val whitespaceP: Parser[Unit] = Parser.charIn(" \t\r\n").void
val whitespacesP: Parser0[Unit] = whitespaceP.rep0.void

val addOperatorP: Parser[AddOperator] = 
Parser.char('+').map(x => PlusOperator) | Parser.char('-').map(x => MinusOperator) |
Parser.string("OR").map(x => OrOperator)

val mulOperatorP: Parser[MulOperator] = 
Parser.char('*').map(x => TimesOperator) | Parser.char('/').map(x => SlashOperator) |
Parser.string("DIV").map(x => DivOperator) | Parser.string("MOD").map(x => ModOperator) |
Parser.char('&').map(x => AndOperator)

val identifierP: Parser[Identifier] = (alpha ~ (alpha | digit).rep0).map((x, xs) => x :: xs).
map(s => Identifier(s.toString))

val hexDigitP: Parser[HexDigit] = Parser.charIn("ABCDEF").map(LetterDigit.apply) | 
digit.map(x => NumberDigit(x.asDigit))

def nonEmptyListToInt(l: NonEmptyList[Char]): Int = l.toList.mkString.toInt

val scaleFactorP: Parser[ScaleFactor] = (Parser.char('E') *> 
(Parser.char('+').map(x => Some(ScalePlus)) | Parser.char('-').map(x => Some(ScaleMinus)) |
Parser.pure(None)) ~ digit.rep.map(nonEmptyListToInt)).map(ScaleFactor.apply)

val realP = (digit.rep.map(nonEmptyListToInt) <* Parser.char('.')).map(x => x.toDouble) ~
digit.rep0 ~ scaleFactorP