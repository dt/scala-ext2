object Directory {

	def scanAndBuildTree(bytes:Bytes) = {
		scanForDirs(bytes, i => {
			val dir = new DirRec( bytes.getFrom( i ) )
			println(dir)
			
			false
		})
	}

	def findRootdir(bytes : Bytes) : Option[Int] = {
		scanForDirs(bytes, i => {
			val d1 = new DirRec( bytes.getFrom( i ) )
			val d2 = new DirRec( bytes.getFrom( i+d1.length ) )
			println("\t "+ d1 )

			var offset = d1.length
			for (d<- 1 to 6) {
					val dir = new DirRec( bytes.getFrom( i+offset ) )
					offset += dir.length
					println("\t "+ dir )
				}
				(d1.nameIsDot && d2.nameIsDotDot)
			})
  }

	def scanForDirs(bytes : Bytes, fn : Int => Boolean) : Option[Int] = {
		var i = 0
		while(i < bytes.length - 8) {
			val d1 = new DirRec( bytes.getFrom( i ) )
			if(d1.length <= DirRec.max_length && d1.nameIsDot) {
				if(fn(i)) return Some(i)
				i += math.max(d1.length - 1, 0)
			}
			i += 1
		}
		None
	}
}
/*
class Directory(val name : String, val parent : Directory, val inode : Inode) {
	var subdirs = List[Directory]()
	var files = List[FsFile]()


}

class RootDirectory extends Directory("/", null, 2) {
	override def parent = this
}
*/

object DirRec {
	val max_length = (0xFF + 8) // 4 + 2 + 1 + 1 + max(nameLength) 
}

class DirRec(bytes : Bytes) {
	def inode = { bytes.get4(0) }
	def length = { bytes.get2(4) }
	def nameLength = { bytes.get1Int(6)  }
	def ftype = { bytes.get1Int(7)  }
	def name = { 
		val sb = new StringBuilder
		for (i <- 0 until nameLength ) //note: 'until', not 'to'
			sb append bytes.get1(8+i)
		sb toString
	}

  // faster checks for . and .. than string compare
	def nameIsDot = {
		nameLength == 1 && bytes.get1(8) == '.'
	}
	def nameIsDotDot = {
		nameLength == 2 && bytes.get1(8) == '.' && bytes.get1(8+1) == '.' 
	}

	override def toString = {
		Hex.valueOf(bytes.trueOffset) +"\t\tinode: "+inode +"\tlen: "+length + "\ttype: " +ftype+ "\tname("+nameLength + "): "+name
	}

}