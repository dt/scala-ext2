package extreader

object DirectoryFinder {

	def findRootdir(fs : FileSystem) : Option[Int] = {
		val bytes = fs.bytes
		scanForDirs(bytes, i => {
			val d1 = new DirRec( bytes.getFrom( i ) )
			val d2 = new DirRec( bytes.getFrom( i+d1.next ) )

			if( (d2.nameIsDotDot && d1.inodeNum == d2.inodeNum)) {
				debug("[DirF]\t"+d1 )
				debug("[DirF]\t"+d2 )
				debug("[DirF]\t1k Block: "+d1.bytes.trueOffset / 1024 + "; 2k block: " + d1.bytes.trueOffset / 2048 )
				debug("")

			}

			false
		})
	}
  

  /**
  	Will invoke fn on each 
  */
	def scanForDirs(bytes : Bytes, fn : Int => Boolean) : Option[Int] = {
		var i = 0
		while(i < bytes.length - DirRec.minLength) {

			val dir = new DirRec( bytes.getFrom( i ) )

			if(dir.next <= DirRec.maxLength && dir.nameIsDot ) {
				
				if( fn(i) )
					return Some(i)
				
			} 
			i += 1
		}
		None
	}
}

object Directory {
	def apply(inode: Inode, name: String) : Directory = {
		val fs = inode.fs
		debug("[dir]\tLoading directory: "+name)
		val dir = new Directory(inode, name)
		debug("[dir]\t"+inode)
		var valid = true

		for( block <- inode.blocks ) {
			if(valid) {
				var i = 0
				while(valid && i < block.length - DirRec.minLength) {
					val rec = new DirRec( block.getFrom(i) )
					
					debug("[dir] Processing: "+rec)

					if(rec.inodeNum != inode.num && rec.inodeNum > 0 && !rec.nameIsDot && !rec.nameIsDotDot) {
						val child = inode.fs.inode(rec.inodeNum)

						if(child.isDir) {
							debug("[dir]\trecursing into child dir "+rec.name)
							dir.subdirs = Directory(child, rec.name) :: dir.subdirs
						}

						if(child.isFile) {
							debug("[dir]\tAdding file "+rec.name)
							dir.files = new FsFile(child, rec.name) :: dir.files
						}
					}

					if(rec.next == 0) {
						valid = false
						debug("[dir]\tlast record")
					} else
						i = i + rec.next
				}
			}
		}
		dir 
	}
}

class Directory(val inode: Inode, val name: String) {
	var subdirs = List[Directory]()
	var files = List[FsFile]()
}

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