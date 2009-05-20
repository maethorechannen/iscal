package com.maethorechannen.iscal

import scala.collection.mutable.ListBuffer

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
