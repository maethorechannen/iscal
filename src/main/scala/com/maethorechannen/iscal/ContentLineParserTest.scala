package com.maethorechannen.iscal
//ceci n'est pas un test
object ContentLineParserTest extends ContentLineParser {
	def main(args: Array[String]) = {
		val t1 = "BEGIN:VCALENDAR"
		val t2 = "DTSTART;TZID=UTC;VALUE=DATE:20091217"
		val t3 = "DTSTART;TZID=UTC,;VALUE=DATE:20091217"

		val p = new ContentLineParser()
		println(t1)
		println(parseAll(contentline, t1))
		println(t2)
		println(parseAll(contentline, t2))
		println(t3)
		println(parseAll(contentline, t3))

	}
}
