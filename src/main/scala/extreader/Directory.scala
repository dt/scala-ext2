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
*	Utility class for finding the directory contents in some bytes
*/
object DirectoryFinder {
	/**
	*	Similar to findRootdir, but does not use blocks	
	*/ 
	def rawFindRootdir(bytes: Bytes) : Option[Long] = {
		var i = 0L
		while(i < bytes.length - 1024) {
			val self = new DirRec( bytes.getFrom(i) )
			if(self.nameIsDot && self.inodeNum==2 && self.next>0 && self.next<20) {
				val parent = new DirRec( bytes.getFrom(i + self.next) )
				if(parent.nameIsDotDot && parent.inodeNum == 2) {
					return(Some(i))
				}
			}
			i+=1
		}
		None
	}

	/**
	*	seaches a filesystem block-by-block, checking for "." and ".." pointing to 2
	*/
	def findRootdir(fs: FileSystem) : Option[Block] = {
		val bytes = fs.bytes
		find(fs, (block, self, parent) => {

			if( self.inodeNum == parent.inodeNum ) {
				debug("[DirF]\t"+self )
				debug("[DirF]\t"+parent )
				true
			} else
				false
		})
	}

	/**
	*	searches a filesystem block-by-block invoking fn on blocks which
	* start with "." and ".." records, passing the block and the records
	*/
	def find(fs: FileSystem, fn : (Block, DirRec, DirRec) => Boolean) : Option[Block] = {
		var i = 0
		for(i <- fs.blocks) {

			val self = new DirRec( i )

			if(self.next <= DirRec.maxLength && self.nameIsDot ) {
				val parent = new DirRec( i getFrom self.next )
				if(parent.next <= fs.blockSize && parent.nameIsDotDot )
				
				if( fn(i,self,parent) )
					return Some(i)
				
			}
		}
		None
	}

	/**
	*	similar to find, but does not pass self and parent to fn
	*/
	def find(fs: FileSystem, fn : Block => Boolean) : Option[Block] = {
		find(fs, (block, self, parent) => fn(block))
	}
}

/**
*	Utility object for methods which handle loading directories from records
*/
object Directory {
	def apply(inode: Inode, name: String ) : Directory = {
		val fs = inode.fs
		debug("[dir "+name+"]\tLoading directory: "+name)
		val dir = new Directory(inode, name)
		debug("[dir "+name+"]\tStarting at inode: "+inode)
		var valid = true
		var next = List[ () => Unit]()

		for( block <- inode.blocks ) {
			if(valid) {
				var i = 0
				while(valid && i < block.length - DirRec.minLength) {
					val rec = new DirRec( block.getFrom(i) )
					
					debug("[dir "+name+"]\tProcessing: "+rec)

					if(rec.inodeNum != inode.num && rec.inodeNum > 0 && !rec.nameIsDot && !rec.nameIsDotDot) {
						val child = inode.fs.inode(rec.inodeNum)
						debug("[dir "+name+"]\t child inode:"+child)

						if(child.isFile) {
							debug("[dir "+name+"]\tAdding file "+rec.name)
							dir.files = new FsFile(child, rec.name) :: dir.files
						}


						if(child.isDir && child.blockCount < 100) {
							debug("[dir "+name+"]\t defering recurse into "+rec.name)
							next = ( () => {
								debug("[dir "+name+"]\trecursing into child dir "+rec.name)
								dir.subdirs = Directory(child, rec.name) :: dir.subdirs
							} ) :: next							
						}


					}

					if(rec.next == 0) {
						valid = false
						debug("[dir "+name+"]\tlast record")
					} else
						i = i + rec.next
					
				}
				for(down <- next) { down() }
			}
		}
		dir 
	}
}


class Directory(val inode: Inode, val name: String) {
	var subdirs = List[Directory]()
	var files = List[FsFile]()
}

/**
*	class for directory records
*/
object DirRec {
	val structLength = 4 + 2 + 1 + 1
	val maxLength = structLength + 0xFF // 8+ max(nameLength) 
	val minLength = structLength + 0 // 8+0
}

class DirRec(val bytes : Bytes) {
	def inodeNum = { bytes.get4(0) }
	def next = { bytes.get2(4) }
	def nameLength = { bytes.get1Int(6)  }
	def ftype = { bytes.get1Int(7)  }
	def name = { 
		val sb = new StringBuilder
		for (i <- 0 until nameLength ) //note: 'until', not 'to'
			sb append bytes.get1(8+i)
		sb toString
	}

	def length = { 
		align(4, DirRec.structLength + nameLength)
	}

  // faster checks for . and .. than string compare
	def nameIsDot = {
		nameLength == 1 && bytes.get1(8) == '.'
	}
	def nameIsDotDot = {
		nameLength == 2 && bytes.get1(8) == '.' && bytes.get1(8+1) == '.' 
	}

	override def toString = {
		hex(bytes.trueOffset) +"\t\tinode: "+inodeNum +"\tlen: "+next + "/" +length + "\ttype: " +ftype+ "\tname("+nameLength + "): '"+name+"'"
	}

}