package extreader


object Inode {
	val size = 128

	def findAllBy( fs: FileSystem, test: Inode => Boolean) = {
		var i = 0
		var res = List[Inode]()
		
		while(i < fs.bytes.length - 127) {
			val inode = fs.fakeInodeAt(i)
			
			try {	
				if(test(inode))
					res = inode :: res
			} catch {
				case aioobe : ArrayIndexOutOfBoundsException => {}
			}
			i += 1
		}
		res
	}

	def findByFirstBlockNum( fs: FileSystem, num: Int , test: Inode => Boolean) = {
		var i = 0
		var res : Option[Inode] = None
		while(i < fs.bytes.length - 4 && res.isEmpty) {
			if( fs.bytes.get4(i) == num ) {
				val inode = fs.fakeInodeAt(i-Inode.firstBlock)
				
				if(inode looksLikeDir) println(inode)
				
				if(test(inode))
					res = Some(inode)
			}
			i += 1

		}
		res
	}

	val firstBlock = 40
}


import util.control.Breaks._


class Inode(val fs : FileSystem, val num: Long, val bytes: Bytes) {
	/**
	*	Offset (bytes)	Size (bytes)	Description
	*	0	2	i_mode
	*	2	2	i_uid
	*	4	4	i_size
	*	8	4	i_atime
	*	12	4	i_ctime
	*	16	4	i_mtime
	*	20	4	i_dtime
	*	24	2	i_gid
	*	26	2	i_links_count
	*	28	4	i_blocks
	*	32	4	i_flags
	*	36	4	i_osd1
	*	40	15 x 4	i_block
	*/
	def mode = bytes.get2(0)
	def format = mode & Constants.EXT2_S_IFMT
	def umask = mode & (0x0777)
	def perms = mode & 0xF000
	def owner = bytes.get2(2)
	def size =  bytes.get4(4)
	def atime = bytes.get4(8)
	def ctime = bytes.get4(12)
	def mtime = bytes.get4(16)
	def dtime = bytes.get4(20)
	def group = bytes.get2(24)
	def linkCount = bytes.get2(26)
	def halfKBlockCount = bytes.get4(28)

	def blockCount = halfKBlockCount /^ (2<<fs.blockSizeExpo)

	val indirectsPerBlock = fs.intsPerBlock
	val doubleIndirectsPerBlock = indirectsPerBlock * indirectsPerBlock
	val tripleIndirectsPerBlock = indirectsPerBlock * indirectsPerBlock * indirectsPerBlock


	def numberOfBlock(localBlockIndex: Int): BlockNum = {
		
		if( localBlockIndex < 12) { // blocks 0 - 11
			println("[inode]\tblock "+localBlockIndex+" is direct")
			bytes.get4(40 + (localBlockIndex * 4))
		} else {
			var index = localBlockIndex - 12

			if( index < indirectsPerBlock ) { // fits in first indirect
				println("[inode]\tblock "+localBlockIndex+" is indirect: "+index)
				val pointersAt = bytes.get4(40 + (12 * 4))
				resolveIndirect(pointersAt, index, 1)
			} else {
				index = index - indirectsPerBlock
				if( index < doubleIndirectsPerBlock ) { //fits in double-indirect
					println("[inode]\tblock "+localBlockIndex+" is dbl indirect: "+index)
					val pointersAt = bytes.get4(40 + (13 * 4))
					resolveIndirect(pointersAt, index, 2)
				} else {
					index = index - doubleIndirectsPerBlock
					if( index < tripleIndirectsPerBlock ) {
					println("[inode]\tblock "+localBlockIndex+" is tri indirect: "+index)
						val pointersAt = bytes.get4(40 + (14 * 4))
						resolveIndirect(pointersAt, index, 3);
					} else {
						throw new IllegalArgumentException("Block index cannot be resolved")
					} 
				}
			}
		}
	}


	def resolveIndirect(pointersBlock: Long, index: Long, level: Int): BlockNum = {
		if(pointersBlock == 0)
			0
		else {
			val pointers = fs.block(pointersBlock)
			if (level == 1)
				pointers.get4(index * 4)
			else {
				val indirectsBelow = math.pow( indirectsPerBlock , level - 1).toInt
				val page = pointers.get4( (index / indirectsBelow) * 4)
      
     	 resolveIndirect(page, index % indirectsBelow, level - 1)
			}
		}
	}

	def blocks = {
		println("[inode]\tReading blocks...")
		var blocks = List[Block]()
		var i = 0
		var valid = true
		while (i < blockCount && valid) {

			val blockNum = numberOfBlock(i)
			println("[inode]\t\t"+i+" -> "+blockNum)

			if(blockNum > 0) {
				blocks = fs.block(blockNum) :: blocks
			} else {
				valid = false
			}
			i = i + 1
		}
		println("[inode]\tDone reading blocks...")
		blocks reverse
		
	}

/*
	def blocks : List[Block] = {
		var blocks = List[Block]()

		var valid = true
		var directOffset = 0

		breakable {
		 //block indirection:
		 //  addr 0 = *(40 + num*4) // direct block, or ptr if num >= 12
		 //  addr 1 = *(addr0 + off1) // indirect block, or ptr if num >= 13
		 //  addr 2 = *(addr1 + off2) // double-indirect block, or ptr if num >= 14
		 //  addr 3 = *(addr2 + off3) //triple-indirect block
			for (offset0 <- 0 to 14) {
				val block0 = bytes.get4(Inode.firstBlock + (offset0 * 4))
				if(addr0 <= 0) break
				
				println("> "+offset0+"\t "+addr0)

				if(offset0 < 12) {
					//direct blocks -- addr0 is pointer to block
					blocks = fs.block(addr0.toInt) :: blocks 
				} else {
					//indirect blocks -- addr0 is pointer to pointers
					for (offset1 <- 0 to fs.intsPerBlock) {
						val addr1 = fs.block.get4(addr0.toInt + (offset1 * 4) )
						if(addr1 <= 0) break;
		
						println(">\t>"+offset1+"\t "+addr1)

						if(offset0 == 12) { //indirect blocks
							blocks = fs.blockAt(addr1.toInt) :: blocks
						} else {
							// go over all entries in dbl-ind blocks
							for (offset2 <- 0 to fs.intsPerBlock) {
								val addr2 = fs.bytes.get4(addr1.toInt + (offset2 * 4) )
								if(addr2 <= 0) break;

								println(">\t>\t>"+offset1+"\t "+addr1)

								if(offset0  == 13) { //dbl-ind blocks
									blocks = fs.block(addr2.toInt) :: blocks
								} else { //trip ind block table
								
									// go over all entries in tri-ind blocks
									for (offset3 <- 0 to fs.intsPerBlock) {
										val addr3 = fs.bytes.get4(addr2.toInt + (offset3 * 4) )
										if(addr3 <= 0) break;										
										blocks = fs.blockAt(addr3.toInt) :: blocks
									} // done going through all tri-ind blocks 
								}
							} // done going through all dbl-ind blocks
						}
					} // done going through all ind blocks
				}
			} //done going though all blocks
		}
		blocks reverse
	}
*/		

	
	 
	 
	 
	 
	 
	
	def looksValid = (format == Constants.EXT2_S_IFLNK || format == Constants.EXT2_S_IFREG || format == Constants.EXT2_S_IFBLK || format == Constants.EXT2_S_IFIFO || format == Constants.EXT2_S_IFDIR || format == Constants.EXT2_S_IFCHR)

	def looksLikeDir = format==Constants.EXT2_S_IFDIR

	override def toString = "["+hex(bytes.trueOffset)+"]\tformat: "+hex(format)+"\towner: "+owner+"\tperms: "+hex(umask)+"\tsize: "+size+"\tlinks: "+linkCount +"\tblocks: "+blockCount
}


class InodeTable(fs: FileSystem) {
	
}