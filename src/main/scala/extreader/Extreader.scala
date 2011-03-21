package object extreader {

	type BlockNum = Long


	def hex(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
	def hex(i:Int):String = "%02X" format i
	def hex(i:Char):String = "%02X" format i
	def hex(i:Byte):String = "%02X" format i
	def hex(i:Long):String = "%02X" format i

	implicit def Long2LongWithRoundUpDiv(x:Long) = new LongWithRoundUpDiv(x) 
	implicit def Long2LongWithIsPowerOf(x:Long) = new LongWithIsPowerOf(x) 

	class LongWithRoundUpDiv(x:Long) {
		def /^(y:Long) = (x + y - 1) / y 
	}

	def debug(msg : => AnyRef) = { if(false) println(msg.toString) }

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