package extreader

class Journal(bytes: Bytes) {
	val sb = new JournalSuperblock(bytes.getRange(0,32))

	def blockSize = sb.blockSize
	def blockCount = sb.blockCount
	def firstJournalBlock = sb.firstJournalBlock
	def firstSeqNum = sb.firstSeqNum
	def firstTransBlock = sb.firstTransBlock
}


object JournalHeader {
	val T_DESCRIPTOR = 0x1L
	val T_COMMIT = 0x2L
	val T_SUPERBLOCK_V1 = 0x3L
	val T_SUPERBLOCK_V2 = 0x4L
	val T_REVOKE = 0x05L
}
class JournalHeader(bytes: Bytes) {
	def signature = bytes.get4BE(0)
	def blockType = bytes.get4BE(4)
	def seqNum = bytes.get4BE(8)

	def isValid = {
		(signature == 0xc03b3998L) &&
		(blockType >= JournalHeader.T_DESCRIPTOR) &&
		(blockType <= JournalHeader.T_REVOKE)
	}
}

class JournalSuperblock(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def blockSize = bytes.get4BE(12)
	def blockCount = bytes.get4BE(16)
	def firstJournalBlock = bytes.get4BE(20)
	def firstSeqNum = bytes.get4BE(24)
	def firstTransBlock = bytes.get4BE(28)
}

class JournalDescriptor(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def fsBlock = bytes.get4BE(12)
	def flags = bytes.get4BE(16)
}

class JournalCommit(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
}

class JournalRevoke(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def size = bytes.get4BE(12)
	def revoked = {
		for( i <- 0 until (size/4).toInt ) {
//			bytes.get4BE(i*4 + 16)
		}
	}
}