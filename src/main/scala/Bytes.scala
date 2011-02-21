import java.io.{FileInputStream, File}

trait Bytes {
	def get4(offset : Int ) : Int
	def get2(offset : Int ) : Int
	def get1(offset : Int ) : Int

	def getRange(base : Int, length : Int) : Bytes
}

object Bytes {
	def apply(data: Array[Char]) = new BytesWrapper(data)

	def readFromFile(file : File) : Bytes = {
		val in = new FileInputStream(file)
	  
	  val bytes = new Array[Byte](file.length.toInt)
	  
    var position = 0
    var bytesRead = 0

    print("Reading file...")

    while (position < file.length && bytesRead >= 0) {
      bytesRead = in.read(bytes, position, bytes.length-position )
      position += bytesRead
    }

    println(" done.")

    in.close

    print("Converting file data...")
    val contents = new BytesWrapper(bytes.map{ b => ( b & 0xFF).toChar })
    println(" done.")

    contents
	}
}

class BytesWrapper( val data : Array[Char] ) extends Bytes {

  def get4( offset : Int ) : Int =
  	make4(data(offset+3),data(offset+2),data(offset+1), data(offset))

  def get2( offset : Int ) : Int =
  	make2(data(offset+1),data(offset))

  def get1 ( offset : Int ) : Int =
    lim( data(offset) )

  def getRange(base : Int, length : Int) = new ByteRange(this, base, length)

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

class ByteRange (val data : Bytes, base : Int, length : Int ) extends Bytes {
	def get4( offset : Int )  = data.get4(check(offset + base))

	def get2( offset : Int ) = data.get2(check(offset + base ))

  def get1( offset : Int ) = data.get1(check(offset + base ))

  def getRange(newBase : Int, newLength : Int) = new ByteRange(data, base + newBase, newLength)

	def check( pos : Int ) = {
		if(pos >= (base + length)) throw new IndexOutOfBoundsException()
		pos
	}
} 