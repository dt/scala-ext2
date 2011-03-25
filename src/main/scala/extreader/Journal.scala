package extreader

class Journal(bytes: Bytes) {
	val sb = new JournalSuperblock(bytes.getRange(0,32))

	def blockSize = sb.blockSize
	def blockCount = sb.blockCount
	def firstJournalBlock = sb.firstJournalBlock
	def firstSeqNum = sb.firstSeqNum
	def firstTransBlock = sb.firstTransBlock

	def block(num: Long) = {
		val offset = num * sb.blockSize
		bytes.getRange(offset, sb.blockSize)
	}
}


/* Table 4.1: Journal administrative block standard header 
 *
 * Range	Description	Values 
 * 0-3		Signature	0xc03b3998 
 * 4-7		Block type  1 Descriptor 
 * 						2 Commit 
 * 						3 Superblock Version1 
 * 						4 Superblock Version 2 
 * 						5 Revoke   
 * 8-11  	Seq num  	Any
 */
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


/* Table 4.2: Journal Superblock 
 *
 * Byte Range	Description 
 * 0-11  		Standard Header 
 * 12-15  		Journal Block Size 
 * 16-19  		Number of Journal blocks 
 * 20-23  		Journal block where the journal actually start 
 * 24-27  		Sequence number of first transaction 
 * 28-31  		Journal block of first transaction
 */
class JournalSuperblock(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def blockSize = bytes.get4BE(12)
	def blockCount = bytes.get4BE(16)
	def firstJournalBlock = bytes.get4BE(20)
	def firstSeqNum = bytes.get4BE(24)
	def firstTransBlock = bytes.get4BE(28)
}

/* Table 4.3: Journal descriptor block 
 * Byte Range  	Description		Values 
 * 0-11  		Header  		See table 1 
 * 12-15  		FS block  		Any 
 * 16-19  		Entry flags  	0x01 Journal block has escaped (Note 1) 
 * 								0x02 Entry has the same UUID as the previous (SAME_UUID) 
 * 								0x04 Block was deleted by current transaction (currently not in use) 
 * 								0x08 Last entry in descriptor block 
 * 20-23  		UUID 			(Does not exist if SAME_UUID flag is set)
 */
class JournalDescriptor(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def fsBlock = bytes.get4BE(12)
	def flags = bytes.get4BE(16)
}

/* Table 4.4: Journal commit block 
 * Byte Range	Description 
 * 0-11  		Standard Header
 */
class JournalCommit(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
}

/* Table 4.5: Journal revoke block 
 * Byte Range	Description 
 * 0-11  		Standard Header 
 * 12-15  		Size in bytes of revoke data 
 * 16-SIZE  	List of 4-byte file system block addresses being revoked 
 */
class JournalRevoke(bytes: Bytes) {
	def header = new JournalHeader(bytes.getRange(0,12))
	def size = bytes.get4BE(12)
	def revoked = {
		for( i <- 0 until (size/4).toInt ) {
//			bytes.get4BE(i*4 + 16)
		}
	}
}