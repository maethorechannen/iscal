package com.maethorechannen.iscal
import scala.util.parsing.combinator._
class ContentLineParser extends RegexParsers {
	def contentline: Parser[Any] = name ~ opt(rep((";" ~ param ))) ~ ":" ~ value ~ crlf
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
	def non_us_ascii: Parser[Any] = "[0x80-0xF8 ]"// Use restricted by charset parameter on outer MIME object (UTF-8 preferred)
	def qsafe_char: Parser[Any] = wsp | "[0x21]" | "[0x23-0x7E]" | non_us_ascii // Any character except CTLs and DQUOTE
	def safe_char: Parser[Any]   = wsp | "[0x21]" | "[0x23-0x2B]" | "[0x2D-0x39]" | "[0x3C-0x7E]" | non_us_ascii // Any character except CTLs, DQUOTE, ";", ":", ","
	def value_char = wsp | "[0x21-0x7E]" | non_us_ascii // Any textual character
	def cr = "[0x0D]" // carriage return
	def lf: Parser[Any] = "[0x0A]" // line feed
	def crlf: Parser[Any] = cr ~ lf // Internet standard newline
	def ctl: Parser[Any] = "[0x00-0x08]" | "[0x0A-0x1F]" | "[0x7F]" //Controls
	def alpha: Parser[Any] = "[0x41-0x5A]" | "[0x61-0x7A]" // A-Z | a-z
	def digit: Parser[Any] = "[0x30-0x039]" // 0-9
    def dquote: Parser[Any]= "[0x22]" // Quotation Mark
    def wsp: Parser[Any] = space | htab
	def space:Parser[Any] = "[0x20]"
	def htab: Parser[Any] = "[0x09]"

}

