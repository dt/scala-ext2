package extreader

class BlockGroupDescTable(val fs: FileSystem) {

	
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

class BlockGroupDesc(val bytes: Bytes) {
	def blockBitmap = bytes.get4(0)		//bg_block_bitmap
	def inodeBitmap = bytes.get4(4)		//bg_inode_bitmap
	def inodeTable = bytes.get4(8)		//bg_inode_table
	def freeBlocks = bytes.get2(12)		//bg_free_blocks_count
	def freeInodes = bytes.get2(14)		//bg_free_inodes_count
}

class BlockGroup {
	
}