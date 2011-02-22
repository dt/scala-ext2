object Directory {
	def findRootdir : Option[Int] = {
		var i = 0
		while(i < bytes.length - length) {
			val d = new Directory( bytes.getRange( i, length ) )
			if ( isRootdir(d) )
				return Some(i)
			i += 1
		}
		None
	}

	def isRootdir(dir : Directory) = {
		
	}
}

class Directory(bytes : Bytes) {
	def inode = { bytes.get4(0) }
	def length = { bytes.get2(4) }
	def nameLength = { bytes.get2(6) }
	def name = { 
		val sb = new StringBuilder
		for (i <- 0 to nameLength )
			sb append bytes.get1(8+i)
		sb toString
	}
}