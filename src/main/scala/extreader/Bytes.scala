package extreader

import java.io.{FileInputStream, File}

trait Bytes {
	def get4(offset : Long ) : Long
	def get2(offset : Long ) : Int
	def get1(offset : Long ) : Char

  def get1Int(offset : Long ) : Int

  def length : Long

  def trueOffset : Long

	def getRange(base : Long, length : Long) : Bytes
	
	def getFrom(base : Long) : Bytes
}

object Bytes {
	def apply(data: Array[Char]) = new ByteArrayWrapper(data)

	def fromFile(file : File) : Bytes = {
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
    val contents = new ByteArrayWrapper(bytes.map{ b => ( b & 0xFF).toChar })
    println(" done.")

    contents
	}
}

class ByteArrayWrapper( val data : Array[Char] ) extends Bytes {
	def trueOffset = 0

	def length = data.length

  implicit def long2int(l: Long) = l.toInt

  def get4( offset : Long ) : Long =
  	make4(data(offset+3),data(offset+2),data(offset+1), data(offset))

  def get2( offset : Long ) : Int =
  	make2(data(offset+1),data(offset))

  def get1 ( offset : Long ) : Char =
    lim( data(offset) )

  def get1Int( offset : Long) : Int = (get1(offset) & 0x00FF).asInstanceOf[Int]

  def getRange(base : Long, length : Long) = new ByteRange(this, base, length)

  def getFrom(base : Long) = new BytesWithOffet( this, base )


  def lim(b : Char) : Char = (b & 0xFF).toChar

  def make2(b1 : Char, b2 : Char) =
    concat2( lim(b1), lim(b2) )

  def make4(b1 : Char, b2 : Char, b3 : Char, b4 : Char) =
    concat4( lim(b1), lim(b2), lim(b3), lim(b4) )

  def concat2( b1 : Char, b2 : Char) : Int = 
    ((b1 << 8) | b2) & 0xFFFF

  def concat4( b1 : Char, b2 : Char, b3 : Char, b4 : Char) : Long =
  	((b1 << 24) | (b2 << 16) | (b3 << 8) | b4) & 0xFFFFFFFFL
}

class BytesWrapper(val data: Bytes) extends Bytes {
  def length = data.length

  def trueOffset = data.trueOffset

  def get4( offset : Long )  = data.get4(offset)

  def get2( offset : Long ) = data.get2(offset)

  def get1( offset : Long ) = data.get1(offset)

  def get1Int( offset : Long ) = data.get1( offset )

  def getRange(newBase : Long, newLength : Long) = new ByteRange(data, newBase, newLength)

  def getFrom(newBase : Long) = new BytesWithOffet( data, newBase )
}

class BytesWithOffet( val data: Bytes, base : Long) extends Bytes {

	def length = data.length - base

	def trueOffset = base + data.trueOffset

	def get4( offset : Long )  = data.get4(offset + base)

	def get2( offset : Long ) = data.get2(offset + base )

  def get1( offset : Long ) = data.get1(offset + base )

  def get1Int( offset : Long ) = data.get1( offset + base )

  def getRange(newBase : Long, newLength : Long) = new ByteRange(data, base + newBase, newLength)

  def getFrom(newBase : Long) = new BytesWithOffet( data, base+newBase )

}

class ByteRange (val data : Bytes, base : Long, count : Long ) extends Bytes {
 	def length = count

	def trueOffset = base + data.trueOffset

	def get4( offset : Long )  = data.get4(check(offset + base))

	def get2( offset : Long ) = data.get2(check(offset + base ))

  def get1( offset : Long ) = data.get1(check(offset + base ))

  def get1Int( offset : Long ) = data.get1( check( offset + base )) 

  def getRange(newBase : Long, newLength : Long) = new ByteRange(data, base + newBase, newLength)

  def getFrom(newBase : Long) = new BytesWithOffet( data, base+newBase )

	def check( pos : Long ) = {
		if(pos >= (base + length)) throw new IndexOutOfBoundsException(pos + " >= ("+base+" + "+length+")")
		pos
	}
}