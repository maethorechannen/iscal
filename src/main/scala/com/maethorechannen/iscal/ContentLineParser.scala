package com.maethorechannen.iscal
import scala.util.parsing.combinator._
class ContentLineParser extends RegexParsers {
	def contentline: Parser[Any] = name ~ opt(rep((";" ~ param ))) ~ ":" ~ value //~ crlf
   // This ABNF is just a general definition for an initial parsing
   // of the content line into its property name, parameter list,
   // and value string
// When parsing a content line, folded lines MUST first
   // be unfolded according to the unfolding procedure
   // described above. When generating a content line, lines
   // longer than 75 octets SHOULD be folded according to
   // the folding procedure described above.
   	def name               = x_name | iana_token
	def iana_token = rep(alpha | digit | "-")
// iCalendar identifier registered with IANA
	def x_name             = "X-" ~ opt(vendorid ~ "-") ~ rep(alpha | digit | "-")
// Reservered for experimental use. Not intended for use in
// released products.

	def vendorid: Parser[Any] = (alpha | digit)~(alpha | digit)~(alpha | digit)~opt(rep(alpha | digit))     //Vendor identification
	def param: Parser[Any]   = param_name ~ "=" ~ param_value ~ opt(rep("," ~ param_value))
   // Each property defines the specific ABNF for the parameters
   // allowed on the property. Refer to specific properties for
  // precise parameter ABNF.
   	def param_name: Parser[Any] = iana_token | x_name
	def param_value: Parser[Any] = paramtext | quoted_string
	def paramtext: Parser[Any] = opt(rep(safe_char))
	def value: Parser[Any] = opt(rep(value_char))
	def quoted_string: Parser[Any] = dquote ~ opt(rep(qsafe_char)) ~ dquote
	def non_us_ascii: Parser[Any] = """[\x80-\xF8]""".r // Use restricted by charset parameter on outer MIME object (UTF-8 preferred)
	def qsafe_char: Parser[Any] = wsp | """[\x21]""".r | """[\x23-\x7E]""".r | non_us_ascii // Any character except CTLs and DQUOTE
	def safe_char: Parser[Any]   = wsp | """[\x21]""".r | """[\x23-\x2B]""".r | """[\x2D-\x39]""".r | """[\x3C-\x7E]""".r | non_us_ascii // Any character except CTLs, DQUOTE, ";", ":", ","
	def value_char: Parser[Any] = wsp | """[\x21-\x7E]""".r | non_us_ascii // Any textual character
	def cr: Parser[Any] = """[\x0D]""".r // carriage return
	def lf: Parser[Any] = """[\x0A]""".r // line feed
	def crlf: Parser[Any] = cr ~ lf // Internet standard newline
	def ctl: Parser[Any] = """[\x00-\x08]""".r | """[\x0A-\x1F]""".r | """[\x7F]""".r //Controls
	def alpha: Parser[Any] = """[\x41-\x5A]""".r | """[\x61-\x7A]""".r // A-Z | a-z
	def digit: Parser[Any] = """[\x30-\x39]""".r // 0-9
    def dquote: Parser[Any]= """[\x22]""".r // Quotation Mark
    def wsp: Parser[Any] = space | htab
	def space:Parser[Any] = """[\x20]""".r
	def htab: Parser[Any] = """[\x09]""".r

}

