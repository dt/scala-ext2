package object extreader {

  def hex(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
  def hex(i:Int):String = "%02X" format i
  def hex(i:Char):String = "%02X" format i
  def hex(i:Byte):String = "%02X" format i
  def hex(i:Long):String = "%02X" format i

  implicit def int2IntWithRoundUpDiv(x:Int) = new IntWithRoundUpDiv(x) 

	class IntWithRoundUpDiv(x:Int) {
		def /^(y:Int) = (x + y - 1) / y 
	}
}