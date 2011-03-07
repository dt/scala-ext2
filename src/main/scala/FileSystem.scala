class FileSystem(val bytes: Bytes) {
	var blockSizeExpo = 1

	def blockSize = 1024 << blockSizeExpo
	val inodeSize = 128

	def intsPerBlock = blockSize / 4

	def blockAt(at : Int) = new Block( bytes.getRange(at, blockSize) )
	def inodeAt(at : Int) = new Inode( this, bytes.getRange(at, inodeSize))
}

class Ext2Fs(bytes: Bytes) extends FileSystem(bytes)