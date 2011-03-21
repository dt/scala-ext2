package extreader

class GroupDescTable(val bytes : Bytes) {
	def apply(groupNum: Long) = {
		val offset = (groupNum * GroupDesc.size)
		debug("[GDT] GD "+groupNum+" at "+offset +" ("+ (bytes.trueOffset + offset) +")") 
		new GroupDesc( bytes.getRange( offset, GroupDesc.size ))
	}
	
}

/*
Table 3-12. Block Group Descriptor Structure
Offset (bytes)	Size (bytes)	Description
	0		4		bg_block_bitmap
	4		4		bg_inode_bitmap
	8		4		bg_inode_table
	12	2		bg_free_blocks_count
	14	2		bg_free_inodes_count
	16	2		bg_used_dirs_count
	18	2		bg_pad
	20	12	bg_reserved
*/

object GroupDesc {
	val size = 4 + 4 + 4 + 2 + 2 + 2 + 2 + 12
	assert(size == 32)
}

class GroupDesc(val bytes: Bytes) {
	val blockBitmapBlock = bytes.get4(0)		//bg_block_bitmap
	val inodeBitmapBlock = bytes.get4(4)		//bg_inode_bitmap
	val inodeTableFirstBlock = bytes.get4(8)		//bg_inode_table
	val freeBlocks = bytes.get2(12)		//bg_free_blocks_count
	val freeInodes = bytes.get2(14)		//bg_free_inodes_count

	debug("[GD]\tLoading desc...")
	debug("[GD]\t\t blockBitmapBlock:\t"+blockBitmapBlock )
	debug("[GD]\t\t inodeBitmapBlock:\t"+ inodeBitmapBlock)
	debug("[GD]\t\t inodeTableFirstBlock:\t"+inodeTableFirstBlock )
	debug("[GD]\t\t freeBlocks:\t"+ freeBlocks)
	debug("[GD]\t\t freeInodes:\t"+ freeInodes)

}

class Group(fs: FileSystem, val num: Long, desc: GroupDesc) {
	def blockBitmapBlock = desc.blockBitmapBlock + fs.groupDescPad
	def inodeBitmapBlock = desc.inodeBitmapBlock + fs.groupDescPad
	def inodeTableFirstBlock = desc.inodeTableFirstBlock + fs.groupDescPad
	def freeBlocks = desc.freeBlocks 
	def freeInodes = desc.freeInodes 

	def inodeBytes(inodeIndexInGroup: Long): Bytes = {
		debug("[Group]\tLocation of "+inodeIndexInGroup+" inode in group "+num+"...")
		val blockOffset = inodeIndexInGroup / fs.inodesPerBlock
		val blockNum = inodeTableFirstBlock + blockOffset

		debug("[Group]\t\tIn the "+blockOffset+"th block of inodes in group (block "+blockNum+")...")

		val inodeIndexInBlock = (inodeIndexInGroup % fs.inodesPerBlock)
		debug("[Group]\t\tThe "+inodeIndexInBlock+" inode of block "+blockNum+"...")
		val offsetInBlock = inodeIndexInBlock * fs.inodeSize 
		debug("[Group]\t\tThus 0x"+hex(offsetInBlock)+" bytes into block "+blockNum+"...")

		fs.block(blockNum).getRange(offsetInBlock, fs.inodeSize)
	}
}