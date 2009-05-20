package com.maethorechannen.iscal
//ceci n'est pas un test
object ContentLineParserTest extends ContentLineParser {
	def main(args: Array[String]) = {
		val t1 = "DTSTART;TZID=UTC;VALUE=DATE:20091217"
		val p = new ContentLineParser()
		println(parseAll(contentline, t1))
	}
}
