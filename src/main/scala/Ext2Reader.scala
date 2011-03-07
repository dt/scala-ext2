import java.io.File
/*
  Some inspiration taken from jNode's EXT2 implementation
  http://gitorious.org/jnode/svn-mirror/trees/master/fs/src/fs/org/jnode/fs/ext2
*/

object Ext2Reader { 
	def main(args: Array[String]) { 
		val image = new File(args(0))
		println("File: "+image.getAbsolutePath)
		val bytes = Bytes fromFile image
		val fs = new Ext2Fs(bytes)
		val searchForSuperblocks = false
		val searchForRootdir = true

	  if(searchForSuperblocks) {
			println("Searching for superblocks...")	  
		  var validSBs = Superblock findAllPossible bytes

		  println("Possible SBs: "+validSBs)
		  for( (i, score) <- validSBs.sortBy( t => t._2 ) ) {
		  	println("Possible Superblock ("+score+") at "+i)
		  	val sb = Superblock.loadFrom(bytes, i) 
		  			println("\tLocation: "+sb.bytes.trueOffset)
		  			println("\tMagic num: "+Hex.valueOf(sb.magicNum))
		 		 		println("\tLooks valid: "+sb.looksValid)
		  			println("\tLog Block Size: "+sb.logBlockSize)
		  			println("\tBlock Size: "+sb.blockSize)
		  			println("\tFirst Block: "+sb.firstBlock)
				
			}
		}

		println("Searching for root directory listing...")
		val root = Directory.findRootdir(fs)
		println("")

		root.map { contents =>
			println( "Found a root directory listing at "+Hex.valueOf(contents) )
			println( "Scanning for an inode which points to this listing...")
			val i2 = Inode.findByFirstBlockAddr(fs, contents, _ => false)

			i2 match {
				case Some(i) => {
					println("Found: "+Hex.valueOf(i.bytes.trueOffset))
					println("\t"+i)
					println("\t\tblocks:"+i.blocks)
				}
				case None => println("Could not find a matching node.")
			}
		}

		val inodes = Inode.findAllBy(fs, x => x.looksLikeDir && x.size == 4096 && x.size <= x.blockCount * 512)
		inodes.map{ x => println(Hex.valueOf(x.bytes.trueOffset)+"\t"+x)}

		

		// Directory.scanAndBuildTree(bytes)
		

	}
}