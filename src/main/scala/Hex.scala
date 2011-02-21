object Hex {

  def valueOf(buf: Array[Byte]): String = buf.map("%02X" format _).mkString
  def valueOf(i:Int):String = "%02X" format i
  def valueOf(i:Char):String = "%02X" format i
  def valueOf(i:Byte):String = "%02X" format i
}