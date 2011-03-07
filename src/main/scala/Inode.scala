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

object Inode {

	def findAllBy( fs: FileSystem, test: Inode => Boolean) = {
		var i = 0
		var res = List[Inode]()
		
		while(i < fs.bytes.length - 127) {
			val inode = fs.inodeAt(i)
			
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

	def findByFirstBlockAddr( fs: FileSystem, addr: Int , test: Inode => Boolean) = {
		var i = 0
		var res : Option[Inode] = None
		while(i < fs.bytes.length - 4 && res.isEmpty) {
			if( fs.bytes.get4(i) == addr ) {
				val inode = fs.inodeAt(i-Inode.firstBlock)
				
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

class Inode(val fs : FileSystem, val bytes: Bytes) {
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
	def blockCount = bytes.get4(28) / (2<<fs.blockSizeExpo)

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
				val addr0 = bytes.get4(Inode.firstBlock + (offset0 * 4))
				if(addr0 <= 0) break
				
				println("> "+offset0+"\t "+addr0)

				if(offset0 < 12) {
					//direct blocks -- addr0 is pointer to block
					blocks = fs.blockAt(addr0.toInt) :: blocks 
				} else {
					//indirect blocks -- addr0 is pointer to pointers
					for (offset1 <- 0 to fs.intsPerBlock) {
						val addr1 = fs.bytes.get4(addr0.toInt + (offset1 * 4) )
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
									blocks = fs.blockAt(addr2.toInt) :: blocks
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

	
	 
	 
	 
	 
	 
	
	def looksValid = (format == Constants.EXT2_S_IFLNK || format == Constants.EXT2_S_IFREG || format == Constants.EXT2_S_IFBLK || format == Constants.EXT2_S_IFIFO || format == Constants.EXT2_S_IFDIR || format == Constants.EXT2_S_IFCHR)

	def looksLikeDir = format==Constants.EXT2_S_IFDIR

	override def toString = "format: "+Hex.valueOf(format)+"\towner: "+owner+"\tperms: "+Hex.valueOf(umask
		)+"\tsize: "+size+"\tlinks: "+linkCount +"\tblocks: "+blockCount
}