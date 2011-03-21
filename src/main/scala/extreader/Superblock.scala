package extreader

object SuperblockFinder {
	def search(bytes: Bytes) : Option[Superblock] = {
		println("Searching for superblocks...")
		var validSBs = allPossible(bytes)

		println("Possible SBs: "+validSBs)
		for( (i, score) <- validSBs.sortBy( t => t._2 ) ) {
			println("Possible Superblock ("+score+") at "+i)
			val sb = Superblock.at(bytes, i)
			println("\tLocation: "+sb.bytes.trueOffset)
			println("\tMagic num: "+hex(sb.magicNum))
			println("\tLooks valid: "+sb.isValid)
			println("\tLog Block Size: "+sb.logBlockSize)
			println("\tBlock Size: "+sb.blockSize)
			println("\tFirst Block: "+sb.firstBlock)
			//todo: interactive q/a to accept as sueprblock
		}
		None
	}

	def allPossible(bytes: Bytes) : List[(Long, Int)] = List[(Long,Int)]()
}

object Superblock {
	val size = 1024
  val defaultOffset = 1024

  def apply(bytes: Bytes) = new Superblock(bytes)

  def in(bytes: Bytes) = {
  	at(bytes, defaultOffset)
  }

	def at(bytes : Bytes, pos : Long ) = {
		new Superblock( bytes.getRange(pos, size ) )
	}

	
}

class Superblock(val bytes : Bytes) {

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
	def revision = { bytes.get4(76) }

	def inodeSize = { 
		if(revision == Constants.EXT2_GOOD_OLD_REV)
			Constants.EXT2_GOOD_OLD_INODE_SIZE
		else
			bytes.get2(88)
	}
  def mtime = { bytes.get4(44) }
  def wtime = { bytes.get4(48) }
  def mnt_count = { bytes.get2(52) }
	def magicNum = { bytes.get2(56) }

	def feature_compat = { bytes.get4(92) }

	//Journaling
	def journalEnabled = { 
		(feature_compat & Constants.EXT3_FEATURE_COMPAT_HAS_JOURNAL) != 0
	}
	def journalInode = { bytes.get4(224) }

	def isValid = { 
		magicNum == 0xEF53 &&
		( firstBlock == 1 && logBlockSize == 0) ||
		( firstBlock == 0 && logBlockSize > 0) &&
		inodeCount > 0 && blockCount > 0 &&
		(inodeCount < inodesPerGroup * (blockCount /^ blocksPerGroup)) &&
		(inodeSize.isPowerOfTwo && inodeSize <= blockSize) 

	}
		
	def looksPossible = { magicNum == 0xEF53 && blocksPerGroup <= (blockSize * 8) }

	def score = {
		var s = 0
		if(magicNum == 0xEF53) s += 2
		if(firstBlock >= 0) s += 1
		if(logBlockSize >= 0 && logBlockSize <= 16) s+= 2
		if(blockCount > 0 ) s += 1
		if(inodeCount > 0 ) s += 1
		s
	}

}