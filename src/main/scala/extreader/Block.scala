package extreader

class Block(val num: Long, var bytes : Bytes ) extends BytesWrapper(bytes) {
	override def toString = "block at "+hex(bytes.trueOffset)
}

object BlockSizeGuesser {
	case class Score(size: Int, hits: Int, misses: Int)

	def checkDirs(bytes:Bytes, sizes: List[Int]) = {
		var i = 0L
		var scores = collection.mutable.Map[Int, Score]()

		while(i < bytes.length - 1024) {
			val self = new DirRec( bytes.getFrom(i) )
			if(self.nameIsDot && self.next > 0 && self.next < 20) {
				val parent = new DirRec( bytes.getFrom(i + self.next) )
				if(parent.nameIsDotDot) {
					// this looks like a directory
					for(size <- sizes) {
						val score = scores.getOrElse(size,Score(size,0,0))
						if(i % size == 0) {
							scores.update(size, Score(size, score.hits + 1, score.misses))						
						} else {
							scores.update(size, Score(size, score.hits, score.misses + 1))
						}
					}
				}
			}
			i+=1
		}
		scores.values.toList.sortBy(x => x.hits - x.misses)
	}

	def search(bytes: Bytes) {
		println("Scanning for directory contents blocks...")
		val scores = checkDirs(bytes, List(1024, 2048, 4096, 8192))
		println("For each size, checked to see if directory contents look aligned:")
		println("\tsize\thits\tmisses")
		for(i <- scores) {
			val col = {
				if(i.misses > 1) 
					Console.RED
				else Console.GREEN
			 }
			println(col+"\t"+i.size+"\t"+i.hits+"\t"+i.misses+Console.WHITE)
		}
	}

}