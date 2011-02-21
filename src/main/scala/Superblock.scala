
object Superblock {
	val length = 1024

	def loadFrom(bytes : Bytes) = {
		new Superblock( bytes.getRange(1024, Superblock.length ) )
	}
}

class Superblock(bytes : Bytes) {

  def inodeCount = { bytes.get4(0) }
  def blockCount = { bytes.get4(4) }
  def firstBlock = { bytes.get4(20) }
	def blockSize = { 1024 << logBlockSize } // = math.pow(1024, logBlockSize)
	def logBlockSize = { bytes.get4(24) }
	def logFragSize = { bytes.get4(28) }
	def fragSize = { 1024 << logFragSize }
	def blocksPerGroup = { bytes.get4(32) }
	def fragsPerGroup = { bytes.get4(36) }
	def inodesPerGroup = { bytes.get4(40) }
  def mtime = { bytes.get4(44) }
  def wtime = { bytes.get4(48) }
  def mnt_count = { bytes.get2(52) }
	def magicNum = { bytes.get2(56) }

}