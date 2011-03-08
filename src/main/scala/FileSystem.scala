package extreader

class FileSystem(val bytes: Bytes) {
	var blockSizeExpo = 1

	var blocksPerGroup = 8192

	var inodesPerGroup = 1920

	var inodeCount = 19200
	var blockCount = 76800

	def blockSize = 1024 << blockSizeExpo
	val inodeSize = 128

	def intsPerBlock = blockSize / 4

	def blockAt(at: Int) = new Block( bytes.getRange(at, blockSize) )
	def inodeAt(at: Int) = new Inode( this, bytes.getRange(at, inodeSize))



	def block(num: Int) = {

	def groupNum(blockNum: Int) = (( blockNum - 1 ) /^ blocksPerGroup) + 1
		
	}
}

class Ext2Fs(bytes: Bytes) extends FileSystem(bytes) {
	def hasValidSuperblock = false

}
