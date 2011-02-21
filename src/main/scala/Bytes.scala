class Bytes( val data : Array[Char] ) {

  def get4( offset : Int ) : Int =
  	make4(data(offset),data(offset+1),data(offset+2), data(offset+3))

  def get2( offset : Int ) : Int =
  	make2(data(offset),data(offset+1))


  def lim(b : Char) : Char = (b & 0xFF).toChar

  def make2(b1 : Char, b2 : Char) =
    concat2( lim(b1), lim(b2) )

  def make4(b1 : Char, b2 : Char, b3 : Char, b4 : Char) =
    concat4( lim(b1), lim(b2), lim(b3), lim(b4) )

  def concat2( b1 : Char, b2 : Char) = 
    (b1 << 8) | b2

  def concat4( b1 : Char, b2 : Char, b3 : Char, b4 : Char) =
  	(b1 << 24) | (b2 << 16) | (b3 << 8) | b4
  
}