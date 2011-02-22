object Directory {
	def findRootdir(bytes : Bytes) : Option[Int] = {
		var i = 0
		while(i < bytes.length - 8) {
			val d = new Directory( bytes.getRange( i, bytes.get2(i+4) ) )
			if ( isRootdir(d) )
				return Some(i)
			i += 1
		}
		None
	}
	def isRootdir(dir : Directory) = {
		false	
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