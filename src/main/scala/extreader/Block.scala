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
* Wraps raw bytes which make up a block, keeping track of the block number too
*/
class Block(fs: FileSystem, val num: Long, var bytes : Bytes ) 
	extends BytesWrapper(bytes) {
	override def toString = "block at "+hex(bytes.trueOffset)

	// get a inode which would be the i'th inode in this block
	def fakeInode(index: Long) = {
		 new Inode( fs, -1, getRange( index*fs.inodeSize, fs.inodeSize))
	}
}

/**
*	Utility object for methods used to guess the block size in use in some bytes
*/
object BlockSizeGuesser {
	case class Score(size: Int, hits: Int, misses: Int)

	// looks through bytes for what look like "." and ".." directory records
	// for each size, check if the beginning of the "." record is block aligned
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

	// searches some bytes for likely block sizes reporting which look likely
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