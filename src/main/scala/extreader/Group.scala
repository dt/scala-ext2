package extreader

class GroupDescTable(val bytes : Bytes) {
	def apply(groupNum: Long) = {
		val offset = (groupNum * GroupDesc.size)
		println("[GDT] GD "+groupNum+" at "+offset +" ("+ (bytes.trueOffset + offset) +")") 
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

	println("[GD]\tLoading desc...")
	println("[GD]\t\t blockBitmapBlock:\t"+blockBitmapBlock )
	println("[GD]\t\t inodeBitmapBlock:\t"+ inodeBitmapBlock)
	println("[GD]\t\t inodeTableFirstBlock:\t"+inodeTableFirstBlock )
	println("[GD]\t\t freeBlocks:\t"+ freeBlocks)
	println("[GD]\t\t freeInodes:\t"+ freeInodes)

}

class Group(fs: FileSystem, val num: Long, desc: GroupDesc) {
	def blockBitmapBlock = desc.blockBitmapBlock
	def inodeBitmapBlock = desc.inodeBitmapBlock
	def inodeTableFirstBlock = desc.inodeTableFirstBlock
	def freeBlocks = desc.freeBlocks
	def freeInodes = desc.freeInodes

	def inodeBytes(inodeIndexInGroup: Long): Bytes = {
		println("[Group]\tLocation of "+inodeIndexInGroup+" inode in group "+num+"...")
		val blockOffset = inodeIndexInGroup / fs.inodesPerBlock
		println("[Group]\t\tIn the "+blockOffset+" block of group...")
		val inodeIndexInBlock = (inodeIndexInGroup % fs.inodesPerBlock)
		println("[Group]\t\tThe "+inodeIndexInBlock+" inode of that block...")
		val offsetInBlock = inodeIndexInBlock * fs.inodeSize
		println("[Group]\t\tThus "+offsetInBlock+" bytes into block "+blockOffset+"...")

		println("[Group]\t\tFirst block of inodes is "+inodeTableFirstBlock)
		fs.block(inodeTableFirstBlock + blockOffset).getRange(offsetInBlock, fs.inodeSize)
	}
}