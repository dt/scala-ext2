// This file is part of ScalaFSR.  ScalaFSR is free software: you can
// redistribute it and/or modify it under the terms of the GNU General Public
// License as published by the Free Software Foundation, version 2.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// (c) David Taylor and Daniel Freeman

package extreader

/**
* Wraps bytes which represent a group descriptor table and provides a method
* to obtain the descriptor for a given group number
*/
class GroupDescTable(val bytes : Bytes) {
	def apply(groupNum: Long) = {
		val offset = (groupNum * GroupDesc.size)
		debug("[GDT] GD "+groupNum+" at "+offset +" ("+ (bytes.trueOffset + offset) +")") 
		new GroupDesc( bytes.getRange( offset, GroupDesc.size ))
	}
	
}

/** 
* GroupDesc interprets bytes as a group descriptor
* 	The following is copied from http://www.nongnu.org/ext2-doc/ext2.html
* 
*		Table 3-12. Block Group Descriptor Structure
*			Offset	Size 	Description
*			0				4			bg_block_bitmap
*			4				4			bg_inode_bitmap
*			8				4			bg_inode_table
*			12			2			bg_free_blocks_count
*			14			2			bg_free_inodes_count
*			16			2			bg_used_dirs_count
*			18			2			bg_pad
*			20			12		bg_reserved
*/
object GroupDesc {
	val size = 4 + 4 + 4 + 2 + 2 + 2 + 2 + 12
}

class GroupDesc(val bytes: Bytes) {
	val blockBitmapBlock = bytes.get4(0)			//	bg_block_bitmap
	val inodeBitmapBlock = bytes.get4(4)			//	bg_inode_bitmap
	val inodeTableFirstBlock = bytes.get4(8)	//	bg_inode_table
	val freeBlocks = bytes.get2(12)						//	bg_free_blocks_count
	val freeInodes = bytes.get2(14)						//	bg_free_inodes_count

	debug("[GD]\tLoading desc...")
	debug("[GD]\t\t blockBitmapBlock:\t"+blockBitmapBlock )
	debug("[GD]\t\t inodeBitmapBlock:\t"+ inodeBitmapBlock)
	debug("[GD]\t\t inodeTableFirstBlock:\t"+inodeTableFirstBlock )
	debug("[GD]\t\t freeBlocks:\t"+ freeBlocks)
	debug("[GD]\t\t freeInodes:\t"+ freeInodes)

}

/**
*	Represents a group, with methods using a group descriptor to find items
*/
class Group(fs: FileSystem, val num: Long, desc: GroupDesc) {
	def blockBitmapBlock = desc.blockBitmapBlock
	def inodeBitmapBlock = desc.inodeBitmapBlock
	def inodeTableFirstBlock = desc.inodeTableFirstBlock
	def freeBlocks = desc.freeBlocks 
	def freeInodes = desc.freeInodes 

	// get the absolute block number of the i'th block of this group
	def blockOf(inodeIndex: Long): BlockNum = {
		val blockOffset = inodeIndex / fs.inodesPerBlock
		val blockNum = 	inodeTableFirstBlock + blockOffset

		debug("[Group]\t\tIn the "+blockOffset+
			"th block of inodes in group (block "+blockNum+")...")

		blockNum
	}

	// get the bytes representing the i'th inode in this group
	def inodeBytes(inodeIndexInGroup: Long): Bytes = {
		debug("[Group]\tInode at offset "+inodeIndexInGroup+" in group "+num+"...")
		val blockNum = blockOf(inodeIndexInGroup)

		val inodeIndexInBlock = Inode.indexInBlock(fs, inodeIndexInGroup )
		debug("[Group]\t\tThe "+inodeIndexInBlock+" inode of block "+blockNum+"...")
		val offsetInBlock = inodeIndexInBlock * fs.inodeSize 

		debug("[Group]\t\tThus 0x"+hex(offsetInBlock)+
			" bytes into block "+blockNum+"...")

		fs.block(blockNum).getRange(offsetInBlock, fs.inodeSize)
	}
}