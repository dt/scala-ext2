package extreader

class FileSystem(val bytes: Bytes, val clean:Option[Bytes]) {
	val metaBytes = clean.getOrElse(bytes)
	val blockCache = collection.mutable.Map[Long, Block]()
	val groupCache = collection.mutable.Map[Long, Group]()

	var blockSizeExpo = 0

	var blocksPerGroup = 8192


	var inodesPerGroup = 1832 // ext3: 1920

	var inodeCount = 12824 // ext3: 19200
	var blockCount = 51200 // ext3: 76800

	var inodeBlocksPerGroup = 229

	var firstDataBlock = 1

	val groups = blockCount / blocksPerGroup

	def blockSize = 1024 << blockSizeExpo
	val inodeSize = 128

	val inodesPerBlock = blockSize / inodeSize

	def intsPerBlock = blockSize / 4

	def sparseMetadata = false
	def metadataInGroup(groupNum: Int) = true

	def groupDescBlock = {
		if(blockSize == 1024 ) 
			metaBlock(2)
		else
			metaBlock(1)
	}

	val gdt = new GroupDescTable(groupDescBlock)

	def block(num: Long): Block = { 
		println("[fs]\tfetch block "+num)
		 
		blockCache.getOrElseUpdate(num, blockAt(num, num * blockSize))
	}

	def metaBlock(num: Long): Block = {
		metaBlockAt(num, num * blockSize)
	}

	def inode(num: Long): Inode = {
		if(num < 1 || num > inodeCount) {
			throw new IllegalArgumentException("Bad Inode Number: "+num)
		}
		println("[fs]\tinode "+num+" is the "+inodeIndexInBlock(num)+" inode in group "+groupNumOfInode(num))
		
		val inodeBytes = group(groupNumOfInode(num)).inodeBytes(inodeIndexInBlock(num))

		new Inode(this, num, inodeBytes)
	}

	def group(num: Long): Group = {
		println("[fs]\tfetch group "+num)
		groupCache.getOrElseUpdate(num, new Group(this, num, gdt(num) ))
	}

	def groupNumOfBlock(blockNum: Long) = (( blockNum - firstDataBlock ) / blocksPerGroup)
	def groupNumOfInode(inodeNum: Long) = (( inodeNum - 1) / inodesPerGroup)
	def inodeIndexInBlock(inodeNum: Long) = ( (inodeNum - 1) % inodesPerGroup )
		
	def blockAt(num: Long, at: Long) = {
		println("[fs]\tblock "+num+" at "+at)
		new Block(num, bytes.getRange(at, blockSize) )
	}

	def metaBlockAt(num: Long, at: Long) = {
		new Block(num, metaBytes.getRange(at, blockSize))
	}
	def fakeInodeAt(at: Long) = new Inode( this, -1, bytes.getRange(at, inodeSize))
}

class Ext2Fs(bytes: Bytes, metaBytes: Option[Bytes]) extends FileSystem(bytes, metaBytes) {
	def hasValidSuperblock = false
}

class Ext3Fs(bytes: Bytes, metaBytes: Option[Bytes]) extends FileSystem(bytes, metaBytes) {
	override def metadataInGroup(groupNum: Int) = {
		if (sparseMetadata)
    	(groupNum <= 1 || (groupNum isPowerOf 3) || (groupNum isPowerOf 5) || (groupNum isPowerOf 7))
    else
    	true
   }

}