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

object FileSystem {
	def apply(bytes: Bytes) = new FileSystem(bytes, Superblock.in(bytes), None )
}

class FileSystem(val bytes: Bytes, sb: Superblock, val clean: Option[Bytes]) {
	var superblock : Option[Superblock] = None

	val blockCache = collection.mutable.Map[Long, Block]()
	val groupCache = collection.mutable.Map[Long, Group]()
	val inodeCache = collection.mutable.Map[Long, Inode]()

	var groupDescPad = 0
	var padGroupBelow = 1


	def blockSizeExpo = sb.logBlockSize

	def blocksPerGroup = sb.blocksPerGroup // ext2: 8192 // 8 * blockSize ?

	def inodesPerGroup = sb.inodesPerGroup // ext2: 1832 // ext3: 1920

	def inodeCount = sb.inodeCount // ext2: 12824 // ext3: 19200
	def blockCount = sb.blockCount // ext2: 51200 // ext3: 76800

	def inodeBlocksPerGroup = sb.inodesPerGroup // ext2: 229

	def firstDataBlock = sb.firstBlock

	def groups = blockCount / blocksPerGroup

	var blockSize = 1024 << blockSizeExpo

	def inodesPerBlock = blockSize / inodeSize

	def intsPerBlock = blockSize / 4

  def inodeSize = sb.inodeSize

	def groupDescBlock = {
		if(blockSize == 1024 ) 
			metaBlock(2)
		else
			metaBlock(1)
	}

	def blocks = new IndexedSeq[Block] {
		def length = blockCount.toInt
		def apply(n: Int) = block(n)
	}

	val gdt = new GroupDescTable(groupDescBlock)

	def block(num: Long): Block = { 
		debug("[fs]\tfetch block "+num)
		blockCache.getOrElseUpdate(num, blockAt(num, num * blockSize))
	}

	def inode(num: Long) = inodeCache.getOrElseUpdate(num, loadInode(num))

	def loadInode(num: Long): Inode = {
		if(num < 1 || num > inodeCount) {
			throw new IllegalArgumentException("Bad Inode Number: "+num)
		}
		debug("[fs]\tinode "+num+" is the "+inodeIndexInBlock(num)+"th inode in group "+groupNumOfInode(num))

		val inodeBytes = group(groupNumOfInode(num)).inodeBytes(inodeIndexInGroup(num))

		new Inode(this, num, inodeBytes)
	}

	def group(num: Long): Group = {
		debug("[fs]\tfetch group "+num)
		groupCache.getOrElseUpdate(num, new Group(this, num, gdt(num) ))
	}

	def groupNumOfBlock(blockNum: Long) = {
		(( blockNum - firstDataBlock ) / blocksPerGroup)}
	def groupNumOfInode(inodeNum: Long) = (( inodeNum - 1) / inodesPerGroup)
	def blockNumOfInode(inodeNum : Long) = {
		group(groupNumOfInode(inodeNum)).blockOf(inodeIndexInBlock(inodeNum))}

	def inodeIndexInGroup(inodeNum: Long) = ( (inodeNum - 1) % inodesPerGroup )
	def inodeIndexInBlock(inodeNum: Long) = ( (inodeNum - 1) % inodesPerBlock )
		
	def blockAt(num: Long, at: Long) = {
		debug("[fs]\tblock "+num+" at "+at)
		new Block(this,num, bytes.getRange(at, blockSize) )
	}

	def metaBlock(num: Long): Block = {
		metaBlockAt(num, num * blockSize)
	}

	def metaBlockAt(num: Long, at: Long) = {
		new Block(this,num, clean.getOrElse(bytes).getRange(at, blockSize))
	}

	def fakeInodeAt(at: Long) = new Inode( this, -1, bytes.getRange(at, inodeSize))
}
