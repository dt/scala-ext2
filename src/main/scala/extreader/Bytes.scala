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

package extreader

import java.io.{FileInputStream, File, OutputStream, ByteArrayOutputStream}

/**
*   low level class for accessing some bytes with helpers to get values like:
*     4 byte int (as a long because java doesn't have unsigned)
*     2 byte short (as an int, see above)
*     1 byte char 
*   as well as sub-ranges of a given length at a position
*
*   An implementation which actually wraps bytes, as well implementations which
*   represent subranges by calling the underlying Bytes after applying an offset
*/
trait Bytes {
	def get4(offset : Long ) : Long
  def get4BE(offset : Long ) : Long
  def get2(offset : Long ) : Int
  def get2BE(offset : Long ) : Int
	def get1(offset : Long ) : Char

  def get1Int(offset : Long ) : Int

  def length : Long

  def trueOffset : Long

	def getRange(base : Long, length : Long) : Bytes
	
  def writeTo(os: OutputStream)

  def writeWithOffsetAndLengthTo(os: OutputStream, offset: Long, length: Long)

	def getFrom(base : Long) : Bytes
}

object Bytes {
	def apply(data: Array[Char]) = new ByteArrayWrapper(data)
//  def apply(data: IndexedSeq[Bytes]) = new BytesSeqWrapper(data)

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

/* class BytesSeqWrapper(seq: IndexedSeq[Bytes]) extends Bytes {
  val offsets = {
    var total = 0;
    seq map { b => 
      total += b.length 
      total
    }
  }

  def indexOf(offset: Long) = {
    var index = 0
    for (sofar <- offsets) {
      if (sofar > offset)
        return index
      else
        index += 1
    }
    return index
  }

  def get(offset: Long): Char = {
    val index = indexOf(offset)

    val skipped = if (index == 0) 0 else offsets(index-1)

    seq(index).get1(offset - skipped)
  }

  lazy val length = sizes reduceRight { _ + _ }


} */

// directly wraps an array of bytes (Char's due to Java's lack of unsigned)
class ByteArrayWrapper( val data : Array[Char] ) extends Bytes {
	def trueOffset = 0

	def length = data.length

  implicit def long2int(l: Long) = l.toInt

  def get4( offset : Long ) : Long =
  	make4(data(offset+3),data(offset+2),data(offset+1), data(offset))

  def get4BE(offset: Long): Long =
    make4(data(offset),data(offset+1),data(offset+2), data(offset+3))

  def get2( offset : Long ) : Int =
  	make2(data(offset+1),data(offset))

  def get2BE( offset : Long ) : Int =
    make2(data(offset),data(offset+1))

  def get1 ( offset : Long ) : Char =
    lim( data(offset) )

  def get1Int( offset : Long) : Int = (get1(offset) & 0x00FF).asInstanceOf[Int]

  def getRange(base : Long, length : Long) = new ByteRange(this, base, length)

  def getFrom(base : Long) = new BytesWithOffet( this, base )

  def writeTo(os: OutputStream) = writeWithOffsetAndLengthTo(os, 0, length)

  def writeWithOffsetAndLengthTo(os: OutputStream, writeOffset: Long, writeLength: Long) = {
    debug("[Bytes]\tWriting "+writeLength+" bytes from "+writeOffset+" to stream...")
    for(i <- 0 until writeLength) {
      os.write(data(i+writeOffset))
    }
  }

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

// anything which sits on top of bytes can extend this to be a Bytes itself
class BytesWrapper(val data: Bytes) extends Bytes {
  def length = data.length

  def trueOffset = data.trueOffset

  def get4( offset : Long )  = data.get4(offset)

  def get4BE( offset : Long )  = data.get4BE(offset)

  def get2( offset : Long ) = data.get2(offset)

  def get2BE( offset : Long )  = data.get2BE(offset)

  def get1( offset : Long ) = data.get1(offset)

  def get1Int( offset : Long ) = data.get1( offset )

  def getRange(newBase : Long, newLength : Long) = new ByteRange(data, newBase, newLength)

  def getFrom(newBase : Long) = new BytesWithOffet( data, newBase )

  def writeTo(os: OutputStream) = data.writeTo(os)

  def writeWithOffsetAndLengthTo(os: OutputStream, writeOffset: Long, writeLength: Long) = { 
    data.writeWithOffsetAndLengthTo(os, writeOffset, writeLength )
  }

}

// offsets accesses to data by a base offset
class BytesWithOffet( val data: Bytes, base : Long) extends Bytes {

	def length = data.length - base

	def trueOffset = base + data.trueOffset

	def get4( offset : Long )  = data.get4(offset + base)

  def get4BE( offset : Long )  = data.get4BE(offset + base)

  def get2( offset : Long ) = data.get2(offset + base )

	def get2BE( offset : Long ) = data.get2BE(offset + base )

  def get1( offset : Long ) = data.get1(offset + base )

  def get1Int( offset : Long ) = data.get1( offset + base )

  def getRange(newBase : Long, newLength : Long) = new ByteRange(data, base + newBase, newLength)

  def getFrom(newBase : Long) = new BytesWithOffet( data, base+newBase )

  def writeTo(os: OutputStream) = writeWithOffsetAndLengthTo(os, 0, length )

  def writeWithOffsetAndLengthTo(os: OutputStream, writeOffset: Long, writeLength: Long) = { 
    data.writeWithOffsetAndLengthTo(os, writeOffset + base, writeLength )
  }

}

// offsets accesses to data by a base offset and enforces length of the subrange
class ByteRange (val data : Bytes, base : Long, count : Long ) extends Bytes {
 	def length = count

	def trueOffset = base + data.trueOffset

	def get4( offset : Long )  = data.get4(check(offset + base))

  def get4BE( offset : Long )  = data.get4BE(check(offset + base))

  def get2( offset : Long ) = data.get2(check(offset + base ))

	def get2BE( offset : Long ) = data.get2BE(check(offset + base ))

  def get1( offset : Long ) = data.get1(check(offset + base ))

  def get1Int( offset : Long ) = data.get1( check( offset + base )) 

  def getRange(newBase : Long, newLength : Long) = new ByteRange(data, base + newBase, newLength)

  def getFrom(newBase : Long) = new BytesWithOffet( data, base+newBase )

	def check( pos : Long ) = {
		if(pos >= (base + length)) throw new IndexOutOfBoundsException(pos + " >= ("+base+" + "+length+")")
		pos
	}

  def writeTo(os: OutputStream) = writeWithOffsetAndLengthTo(os, 0, length )

  def writeWithOffsetAndLengthTo(os: OutputStream, writeOffset: Long, writeLength: Long) = { 
    data.writeWithOffsetAndLengthTo(os, check(writeOffset + base), writeLength )
  }

}