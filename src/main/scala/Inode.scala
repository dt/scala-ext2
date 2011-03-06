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
import util.control.Breaks._

class Inode(val fs : FileSystem, val bytes: Bytes) {
	def mode = bytes.get2(0)
	def owner = bytes.get2(2)
	def size =  bytes.get4(4)
	def atime = bytes.get4(8)
	def ctime = bytes.get4(12)
	def mtime = bytes.get4(16)
	def dtime = bytes.get4(20)
	def group = bytes.get2(24)
	def linkCount = bytes.get2(26)

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
				val addr0 = bytes.get4(40 + (offset0 * 4))
				if(addr0 <= 0) break
				
				if(offset0 < 12) {
					//direct blocks -- addr0 is pointer to block
					blocks = fs.blockAt(addr0.toInt) :: blocks 
				} else {
					//indirect blocks -- addr0 is pointer to pointers
					for (offset1 <- 0 to fs.intsPerBlock) {
						val addr1 = fs.bytes.get4(addr0.toInt + (offset1 * 4) )
						if(addr1 <= 0) break;

						if(offset0 == 12) { //indirect blocks
							blocks = fs.blockAt(addr1.toInt) :: blocks
						} else {
							// go over all entries in dbl-ind blocks
							for (offset2 <- 0 to fs.intsPerBlock) {
								val addr2 = fs.bytes.get4(addr1.toInt + (offset2 * 4) )
								if(addr2 <= 0) break;

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
		blocks
	}	
}