// This file is part of ScalaFSR.  ScalaFSR is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, version 2.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// (c) David Taylor and Daniel Freeman

package object extreader {

	type BlockNum = Long

	// global flag for showing debug messages
	var showDebug = false

	// helpers to format any value as hex
	def hex(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
	def hex(i:Int):String = "%02X" format i
	def hex(i:Char):String = "%02X" format i
	def hex(i:Byte):String = "%02X" format i
	def hex(i:Long):String = "%02X" format i

	def align(i: Int, d: Int) = (d + d%i)
	def align(i: Int, d: Long) = (d + d%i)

	// suppress debug messages from being printed during evaluation of block
	def debugOff[T](block: => T) = {
		val oldDebug = showDebug
		showDebug = false
		val result = block
		showDebug = oldDebug
		result
	}

	// decorators for basic types
	implicit def Long2LongWithRoundUpDiv(x:Long) = new LongWithRoundUpDiv(x) 
	implicit def Long2LongWithIsPowerOf(x:Long) = new LongWithIsPowerOf(x) 
	implicit def Char2CharWithBit(x:Char) = new CharWithBit(x) 

	// division which rounds up, not down
	class LongWithRoundUpDiv(x:Long) {
		def /^(y:Long) = (x + y - 1) / y 
	}

	// get the i'th bit of a char
	class CharWithBit(x: Char) {
			def bit(i: Int) = ((x >> i) & 0x1) 
	}

	// conditionally print a debug message
	def debug(msg : => AnyRef) = { if(showDebug) println(msg.toString) }

	class LongWithIsPowerOf(x:Long) { 
		def isPowerOfTwo = {
			( x != 0 && ( x & (x - 1) ) == 0 )
		}

		def isPowerOf(b:Long): Boolean = {
			var a = x
			
			if(b < 1)
				return false

			if(b == 1)
				return a==b

			while ( a >= b) {
				if (a == b)
					return true
				
				if (a % b == 0) {
					a = a / b
				} else {
					return false
				}
			}
			return false
		}
	}
}