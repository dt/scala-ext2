package extreader

object Superblock {
	val length = 1024
  var firstPos = 1024

	def bestGuess(bytes : Bytes) = {
		val first = loadFrom(bytes, firstPos)
		if (first looksValid) {
			first
		} else {
			println("Could not load first superblock...")
			findValid(bytes, firstPos + length) match {
				case Some(i) => {
					println("Found a valid superblock at "+i)
					loadFrom(bytes, i)
				}
				case None => throw new RuntimeException("Cannot find a valid superblock")
			}
		}
	}

	def loadFrom(bytes : Bytes, pos : Int ) = {
		new Superblock( bytes.getRange(pos, length ) )
	}

  def findAllPossible(bytes : Bytes) = {
  	var results = List[(Int,Int)]()
  	var i = firstPos
  	var sb = findPossible(bytes, i)

  	while( sb isDefined ) {
  		i = sb.get._1
  		val score = sb.get._2
  		//println("\tpossible superblock at "+i)
  		results = (i, score) :: results
  		sb = findPossible( bytes, i+1 )
  	}
  	results
  }

	def find(bytes : Bytes) : Option[Int] = findValid(bytes, 1024)

	def findValid(bytes : Bytes, start : Int) : Option[Int] = {
		var i = start
		while(i < bytes.length - length) {
			val sb = new Superblock( bytes.getRange( i, length ) )
			if ( sb looksValid )
				return Some(i)
			i += 1
		}
		None
	}

	def findPossible(bytes : Bytes, start : Int) : Option[(Int, Int)] = {
		var i = start
		while(i < bytes.length - length) {
			val sb = new Superblock( bytes.getRange( i, length ) )
			if ( sb looksPossible )
				return Some(i, sb.score)
			i += 1
		}
		None
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
  def mtime = { bytes.get4(44) }
  def wtime = { bytes.get4(48) }
  def mnt_count = { bytes.get2(52) }
	def magicNum = { bytes.get2(56) }

	def looksValid = { 
		magicNum == 0xEF53 &&
		firstBlock > 0 &&
		logBlockSize >= 0 && logBlockSize < 128 &&
		inodeCount > 0 && blockCount > 0 }
		
	def looksPossible = { magicNum == 0xEF53 }

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