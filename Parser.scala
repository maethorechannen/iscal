import scala.collection.mutable.ListBuffer

class ContentLineParser {
def contentline        = name *(";" param ) ":" value crlf
   // This ABNF is just a general definition for an initial parsing
   // of the content line into its property name, parameter list,
   // and value string
// When parsing a content line, folded lines MUST first
   // be unfolded according to the unfolding procedure
   // described above. When generating a content line, lines
   // longer than 75 octets SHOULD be folded according to
   // the folding procedure described above.
def name               = x-name / iana-token
def iana-token = 1*(ALPHA / DIGIT / "-")
// iCalendar identifier registered with IANA
	def x-name             = "X-" [vendorid "-"] 1*(ALPHA / DIGIT / "-")
// Reservered for experimental use. Not intended for use in
// released products.

	def vendorid    = 3*(ALPHA / DIGIT)     ;Vendor identification
	def param               = param-name "=" param-value *("," param-value)
   // Each property defines the specific ABNF for the parameters
   // allowed on the property. Refer to specific properties for
  // precise parameter ABNF.
   	def param-name = iana-token / x-token
	def param-value         = paramtext / quoted-string
	def paramtext   = *SAFE-CHAR
	def value       = *VALUE-CHAR
	def quoted_string       = DQUOTE *QSAFE-CHAR DQUOTE
	def non_us_ascii: Parser[Any] = %x80-F8 // Use restricted by charset parameter on outer MIME object (UTF-8 preferred)
	def qsafe_char: Parser[Any] = wsp | 0x21 | 0x23-0x7E | non_us_ascii // Any character except CTLs and DQUOTE
	def safe_char: Parser[Any]   = wsp | 0x21 | 0x23-0x2B | 0x2D-0x39 | 0x3C-0x7E | non_us_ascii // Any character except CTLs, DQUOTE, ";", ":", ","
	def value_char = wsp | 0x21-0x7E | non_us_asii // Any textual character
	def cr = %x0D // carriage return
	def lf: Parser[Any] = 0x0A // line feed
	def crlf: Parser[Any] = cr lf // Internet standard newline
	def ctl: Parser[Any] = 0x00-0x08 | 0x0A-0x1F | 0x7F //Controls
	def alpha: Parser[Any] = 0x41-0x5A | 0x61-0x7A // A-Z | a-z
	def digit: Parser[Any] = 0x30-0x039 // 0-9
    	def dquote: Parser[Ay]= 0x22 // Quotation Mark
    	def wsp: Parser[Any] = space | htab
	def space:Parser[Any] = 0x20
	def htab: Parser[Any] = 0x09

}

class Parser {
	/** unfold - takes an iterator of folded lines and returns a tuple of(String, Map[String], String)
	
	*/
	def unfold(lines: Iterator[String]): List[String] = {
		def linelexer(line: String) = {
			
		}
		
		def unfolder(unfolded: ListBuffer[String], current: String, lines: List[String]): List[String] = {
			if (lines isEmpty) {
				unfolded.append(current)
				unfolded.toList
			} else {
				val h = lines.head
				if (h.startsWith(" ") == true) {
					unfolder(unfolded, current + h, lines.tail)
				} else {
					unfolded.append(current)
					unfolder(unfolded, h, lines.tail)
				}
			}
	
		}
		val llines = lines.toList
		unfolder(new ListBuffer[String](), llines head, llines tail)
		}
}
