package extreader

class JournalHeader(bytes: Bytes) {
	def signature = bytes.get4(0)
	def blockType = bytes.get4(4)
	def seqNum = bytes.get4(4)
}

class JournalSuperblock(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def blockSize = bytes.get4(12)
	def blockCount = bytes.get4(16)
	def firstJournalBlock = bytes.get4(20)
	def firstSeqNum = bytes.get4(24)
	def firstTransBlock = bytes.get4(28)
}

class JournalDescriptor(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def fsBlock = bytes.get4(12)
	def flags = bytes.get4(16)
}

class JournalCommit(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
}

class JournalRevoke(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def size = bytes.get4(12)
	def revoked = {
		for( i <- 0 until (size/4).toInt ) {
//			bytes.get4(i*4 + 16)
		}
	}
}