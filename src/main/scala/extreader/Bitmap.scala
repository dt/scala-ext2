package extreader

class Bitmap(val block : Block ) {
	def bit(i: Long) = block.get1(i / 8).bit( i.toInt % 8 )
	def set(i: Long) = (bit(i) == 1)
}